-- | Page runtime with an injectable device backend for hardware-independent tests.
module StreamDeck.Pages.Runtime (PageDriver (..), runPageRuntime) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (link, withAsync)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Concurrent.STM
    ( TQueue, TVar, atomically, check, modifyTVar', newTQueueIO, newTVarIO
    , readTQueue, readTVar, readTVarIO, registerDelay, writeTQueue, writeTVar )
import Control.Exception (SomeAsyncException, SomeException, evaluate, fromException, throwIO, try)
import Control.Monad (forM, forever, unless, when)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import StreamDeck.Action (performEffect)
import StreamDeck.Image (KeyImage, imageBytes, prepareImageReports, solidColor)
import StreamDeck.Pages
import StreamDeck.Protocol (Key, KeyEvent (..), keyAt)
import System.IO (hPutStrLn, stderr)

data PageDriver = PageDriver
    { pollPageEvents :: IO [KeyEvent]
    -- | Drain pending reports, preserving held/released key tracking.
    , flushPageEvents :: IO ()
    -- | Send all changed keys as one uninterrupted burst. The runtime pauses
    -- polling for this entire callback, not separately for each key/packet.
    , drawPageFrame :: [(Key, KeyImage)] -> IO ()
    }

type CacheKey = (String, String, String)
type Frame = Map.Map Key BS.ByteString
type RenderCache = Map.Map CacheKey KeyImage
type InitialResults = TVar (Map.Map CacheKey (Either String KeyImage))

blankImage :: KeyImage
blankImage = solidColor 0 0 0

data Event
    = Input Integer [KeyEvent]
    | Rendered Integer CacheKey Key KeyImage
    | RenderFailed Integer String

-- | The observer runs after a complete page/state activation and input flush.
-- Slow observers block UI dispatch; the normal device entry point uses a no-op.
runPageRuntime :: PageDriver -> (DeckState -> IO ()) -> PageConfig -> IO ()
runPageRuntime driver observe config = do
    events <- newTQueueIO
    effects <- newTQueueIO
    gate <- newTVarIO (0 :: Integer, False)
    inputLock <- newMVar ()
    let inputLoop = forever $ withMVar inputLock $ \_ -> do
            before <- readTVarIO gate
            changes <- pollPageEvents driver
            after <- readTVarIO gate
            when (snd before && before == after && not (null changes)) $
                atomically (writeTQueue events (Input (fst before) changes))
        effectLoop = forever $ do
            action <- atomically (readTQueue effects)
            outcome <- attempt (performEffect action)
            either (reportError "action") pure outcome
        serve generation model previous cached = do
            atomically (writeTVar gate (generation, False))
            let visible = visibleButtons config model
                dynamicCount = length [() | (_, view) <- visible, RefreshEvery _ _ <- [stateAppearance view]]
            initial <- newTVarIO Map.empty
            publish <- newTVarIO False
            -- One deadline for the entire page, including renderer startup and
            -- static preparation. Slow jobs continue after the first transfer.
            expired <- if dynamicCount == 0 then newTVarIO True else registerDelay 50000
            (next, nextFrame, nextCache) <- withRenderers events initial publish generation model visible $ do
                mapM_ (\(entry, view) -> case stateAppearance view of
                    Static image -> prepareImageReports (pageButtonKey entry) image >> pure ()
                    RefreshEvery _ _ -> pure ()) visible
                fresh <- atomically $ do
                    results <- readTVar initial
                    deadline <- readTVar expired
                    check (deadline || Map.size results == dynamicCount)
                    -- This atomic handoff prevents a result that arrives during
                    -- frame transfer from being lost until its next interval.
                    writeTVar publish True
                    pure (Map.mapMaybe (either (const Nothing) Just) results)
                let preparedCache = Map.union fresh cached
                (changes, frame) <- prepareFrame visible model previous preparedCache
                -- Exclude reads for the complete transfer and input flush.
                withMVar inputLock $ \_ -> do
                    unless (null changes) (drawPageFrame driver changes)
                    flushPageEvents driver
                    atomically (writeTVar gate (generation, True))
                    observe model
                receive generation model frame preparedCache
            serve (generation + 1) next nextFrame nextCache
        receive generation model frame cached = do
            event <- atomically (readTQueue events)
            case event of
                Input stamp changes | stamp == generation -> dispatch changes
                Rendered stamp cacheKey key image | stamp == generation -> do
                    updated <- drawChanged
                        (\changes -> withMVar inputLock (\_ -> drawPageFrame driver changes)) frame key image
                    receive generation model updated (Map.insert cacheKey image cached)
                RenderFailed stamp message | stamp == generation -> do
                    hPutStrLn stderr ("Stream Deck appearance failed: " ++ message)
                    receive generation model frame cached
                _ -> receive generation model frame cached
          where
            dispatch [] = receive generation model frame cached
            dispatch (Released _ : rest) = dispatch rest
            dispatch (Pressed key : rest) = do
                let (next, queued) = pressKey config key model
                atomically $ do
                    mapM_ (writeTQueue effects) queued
                    when (next /= model) (writeTVar gate (generation + 1, False))
                if next == model
                    then dispatch rest
                    -- Remaining presses in this report describe the old layout.
                    else pure (next, frame, cached)
    withAsync inputLoop $ \reader -> withAsync effectLoop $ \worker -> do
        link reader
        link worker
        serve 0 (initialDeckState config) Map.empty Map.empty

-- JPEGs and cached packet streams are forced before any hardware update starts.
prepareFrame :: [(PageButton, ButtonState)] -> DeckState -> Frame -> RenderCache -> IO ([(Key, KeyImage)], Frame)
prepareFrame visible model previous cached = do
    planned <- forM allKeys $ \key -> do
        let image = Map.findWithDefault blankImage key images
            bytes = imageBytes image
        _ <- evaluate (BS.length bytes)
        let changed = Map.lookup key previous /= Just bytes
        when changed (prepareImageReports key image >> pure ())
        pure (key, image, bytes, changed)
    pure ([(key, image) | (key, image, _, True) <- planned],
          Map.fromList [(key, bytes) | (key, _, bytes, _) <- planned])
  where
    images = Map.fromList
        [ (pageButtonKey entry, case stateAppearance view of
                Static image -> image
                RefreshEvery _ _ -> Map.findWithDefault blankImage (reference model entry view) cached)
        | (entry, view) <- visible ]
    allKeys = [key | row <- [0..2], column <- [0..4], Right key <- [keyAt row column]]

drawChanged :: ([(Key, KeyImage)] -> IO ()) -> Frame -> Key -> KeyImage -> IO Frame
drawChanged present frame key image = do
    let bytes = imageBytes image
    if Map.lookup key frame == Just bytes then pure frame else do
        _ <- prepareImageReports key image
        present [(key, image)]
        pure (Map.insert key bytes frame)

withRenderers :: TQueue Event -> InitialResults -> TVar Bool -> Integer -> DeckState -> [(PageButton, ButtonState)] -> IO a -> IO a
withRenderers events initial publish generation model visible use = launch jobs
  where
    jobs =
        [ forever $ do
            outcome <- attempt $ do
                image <- render
                _ <- prepareImageReports (pageButtonKey entry) image
                pure image
            atomically $ do
                live <- readTVar publish
                let cacheKey = reference model entry view
                case outcome of
                    Left err -> do
                        unless live $ modifyTVar' initial (Map.insertWith keepGood cacheKey (Left (show err)))
                        writeTQueue events (RenderFailed generation (show err))
                    Right image -> if live
                        then writeTQueue events (Rendered generation cacheKey (pageButtonKey entry) image)
                        else modifyTVar' initial (Map.insert cacheKey (Right image))
            threadDelay (max 1 (ceiling (toRational seconds * 1000000)))
        | (entry, view) <- visible
        , RefreshEvery seconds render <- [stateAppearance view] ]
    launch [] = use
    launch (job : rest) = withAsync job $ \worker -> link worker >> launch rest
    keepGood latest old = case old of Right _ -> old; Left _ -> latest

reference :: DeckState -> PageButton -> ButtonState -> CacheKey
reference model entry view = (currentPage model, pageButtonId entry, stateId view)

attempt :: IO a -> IO (Either SomeException a)
attempt operation = do
    outcome <- try operation
    case outcome of
        Left err | Just _ <- (fromException err :: Maybe SomeAsyncException) -> throwIO err
        _ -> pure outcome

reportError :: String -> SomeException -> IO ()
reportError source err = hPutStrLn stderr ("Stream Deck " ++ source ++ " failed: " ++ show err)
