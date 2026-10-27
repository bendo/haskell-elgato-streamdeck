-- | Page runtime with an injectable device backend for hardware-independent tests.
module StreamDeck.Pages.Runtime (PageDriver (..), runPageRuntime) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (link, withAsync)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Concurrent.STM
    ( TQueue, atomically, newTQueueIO, newTVarIO, readTQueue, readTVarIO, writeTQueue, writeTVar )
import Control.Exception (SomeAsyncException, SomeException, evaluate, fromException, throwIO, try)
import Control.Monad (foldM, forever, when)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import StreamDeck.Action (performEffect)
import StreamDeck.Image (KeyImage, imageBytes, solidColor)
import StreamDeck.Pages
import StreamDeck.Protocol (Key, KeyEvent (..), keyAt)
import System.IO (hPutStrLn, stderr)

data PageDriver = PageDriver
    { pollPageEvents :: IO [KeyEvent]
    -- | Drain pending reports, preserving held/released key tracking.
    , flushPageEvents :: IO ()
    , drawPageKey :: Key -> KeyImage -> IO ()
    }

type CacheKey = (String, String, String)
type Frame = Map.Map Key BS.ByteString
type RenderCache = Map.Map CacheKey KeyImage

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
            frame <- paint driver visible model previous cached
            -- No read can straddle this flush and activation. Old packets cannot
            -- become a fresh press on a newly displayed layout.
            withMVar inputLock $ \_ -> do
                flushPageEvents driver
                atomically (writeTVar gate (generation, True))
                observe model
            (next, nextFrame, nextCache) <- withRenderers events generation model visible $
                receive generation model frame cached
            serve (generation + 1) next nextFrame nextCache
        receive generation model frame cached = do
            event <- atomically (readTQueue events)
            case event of
                Input stamp changes | stamp == generation -> dispatch changes
                Rendered stamp cacheKey key image | stamp == generation -> do
                    updated <- drawChanged driver frame key image
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

paint :: PageDriver -> [(PageButton, ButtonState)] -> DeckState -> Frame -> RenderCache -> IO Frame
paint driver visible model previous cached = foldM draw previous allKeys
  where
    blank = solidColor 0 0 0
    images = Map.fromList
        [ (pageButtonKey entry, case stateAppearance view of
                Static image -> image
                RefreshEvery _ _ -> Map.findWithDefault blank (reference model entry view) cached)
        | (entry, view) <- visible ]
    draw frame key = drawChanged driver frame key (Map.findWithDefault blank key images)
    allKeys = [key | row <- [0..2], column <- [0..4], Right key <- [keyAt row column]]

drawChanged :: PageDriver -> Frame -> Key -> KeyImage -> IO Frame
drawChanged driver frame key image = do
    let bytes = imageBytes image
    if Map.lookup key frame == Just bytes then pure frame else do
        drawPageKey driver key image
        pure (Map.insert key bytes frame)

withRenderers :: TQueue Event -> Integer -> DeckState -> [(PageButton, ButtonState)] -> IO a -> IO a
withRenderers events generation model visible use = launch jobs
  where
    jobs =
        [ forever $ do
            outcome <- attempt $ do
                image <- render
                _ <- evaluate (BS.length (imageBytes image))
                pure image
            atomically $ writeTQueue events $ case outcome of
                Left err -> RenderFailed generation (show err)
                Right image -> Rendered generation (reference model entry view) (pageButtonKey entry) image
            threadDelay (max 1 (ceiling (toRational seconds * 1000000)))
        | (entry, view) <- visible
        , RefreshEvery seconds render <- [stateAppearance view] ]
    launch [] = use
    launch (job : rest) = withAsync job $ \worker -> link worker >> launch rest

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
