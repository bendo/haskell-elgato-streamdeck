module PageSpec (tests) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Concurrent.Async (link, withAsync)
import Control.Concurrent.STM
    ( TQueue, TVar, atomically, check, modifyTVar', newTQueueIO, newTVarIO
    , orElse, readTQueue, readTVar, readTVarIO, registerDelay, tryReadTQueue, writeTQueue )
import Control.Exception (IOException, finally, try)
import Control.Monad (forM_, unless, when)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import StreamDeck (runPages)
import StreamDeck.Action
import StreamDeck.Clock
import StreamDeck.Image (KeyImage, imageBytes, solidColor)
import StreamDeck.Pages
import StreamDeck.Pages.Runtime
import StreamDeck.Protocol (Key, KeyEvent (..), keyAt)
import System.Info (os)
import System.Timeout (timeout)
import Test.HUnit (Test (..), (~:), assertBool, assertEqual, assertFailure)

tests :: Test
tests = TestList
    [ "page configuration validation" ~: TestCase configurationValidation
    , "pure navigation and remembered per-page states" ~: TestCase rememberedStates
    , "state actions in sequences target the original button" ~: TestCase sequenceActions
    , "key chord validation" ~: TestCase $ do
        assertEqual "Super+d" (Right "Super_L+d") (keyChordText (super 'd'))
        assertEqual "multiple modifiers" (Right "Control_L+Alt_L+Delete")
            (keyChordText (KeyChord [Control, Alt] "Delete"))
        forM_ [KeyChord [] "", KeyChord [Super, Super] "d", KeyChord [] "-bad",
               KeyChord [] "a+b", KeyChord [] "two words"] $ \chord ->
            assertBool "reject malformed chord" (isLeft (keyChordText chord))
    , "commands and shell effects" ~: TestCase $ when (os /= "mingw32") $ do
        performEffect (Execute "sh" ["-c", "exit 0"])
        performEffect (Shell "exit 0")
        failed <- try (performEffect (Execute "sh" ["-c", "exit 7"])) :: IO (Either IOException ())
        assertBool "nonzero command reports an error" (isLeft failed)
    , "IANA clock daylight saving and date rollover" ~: TestCase $ do
        london <- loadClockZone "Europe/London"
        sydney <- loadClockZone "Australia/Sydney"
        let winter = UTCTime (fromGregorian 2026 1 15) (secondsToDiffTime (12 * 3600))
            summer = UTCTime (fromGregorian 2026 7 15) (secondsToDiffTime (12 * 3600))
            late = UTCTime (fromGregorian 2026 1 15) (secondsToDiffTime (23 * 3600))
        assertEqual "London winter" "1200" (formatClock london "%H%M" winter)
        assertEqual "London summer" "1300" (formatClock london "%H%M" summer)
        assertEqual "Sydney summer" "2300" (formatClock sydney "%H%M" winter)
        assertEqual "Sydney winter" "2200" (formatClock sydney "%H%M" summer)
        assertEqual "Sydney next date" "2026-01-16 1000" (formatClock sydney "%Y-%m-%d %H%M" late)
    , "runtime repaints and clears unused keys" ~: TestCase $ within $ do
        harness <- newHarness
        withRuntime harness rememberedLayout $ do
            initial <- ready harness
            assertEqual "initial page" "main" (currentPage initial)
            assertEqual "all 15 keys initialized" 15 . Map.size =<< readTVarIO (screen harness)
            assertImage harness k0 red
            press harness k1
            weather <- ready harness
            assertEqual "navigated" "weather" (currentPage weather)
            assertImage harness k1 black
            assertImage harness k0 yellow
            assertImage harness k14 blue
            press harness k14
            _ <- ready harness
            assertImage harness k0 red
            assertImage harness k14 black
            assertEqual "flush on every activation" 3 =<< readTVarIO (flushCount harness)
    , "runtime remembers state across page changes" ~: TestCase $ within $ do
        harness <- newHarness
        withRuntime harness rememberedLayout $ do
            _ <- ready harness
            press harness k0
            changed <- ready harness
            assertEqual "state two" (Just "two") (selectedState changed "main" "clock")
            assertImage harness k0 green
            press harness k1
            _ <- ready harness
            press harness k14
            returned <- ready harness
            assertEqual "state retained" (Just "two") (selectedState returned "main" "clock")
            assertImage harness k0 green
            press harness k0
            _ <- ready harness
            assertImage harness k0 blue
            press harness k0
            _ <- ready harness
            assertImage harness k0 red
    , "slow effects do not block navigation" ~: TestCase $ within $ do
        harness <- newHarness
        started <- newEmptyMVar
        blocked <- newEmptyMVar
        cleaned <- newEmptyMVar
        let slow = RunIO ((putMVar started () >> takeMVar blocked) `finally` putMVar cleaned ())
            pages = [Page "main" [button "slow" k0 red slow, button "next" k1 green (SwitchPage "other")],
                     Page "other" []]
        withRuntime harness pages $ do
            _ <- ready harness
            press harness k0
            takeMVar started
            press harness k1
            next <- ready harness
            assertEqual "navigation works during blocked action" "other" (currentPage next)
        takeMVar cleaned
    , "failed effects do not stop later actions" ~: TestCase $ within $ do
        harness <- newHarness
        completed <- newEmptyMVar
        let pages = [Page "main" [button "fail" k0 red (Sequence
                [RunIO (ioError (userError "expected page action failure")), RunIO (putMVar completed ())])]]
        withRuntime harness pages $ do
            _ <- ready harness
            press harness k0
            takeMVar completed
    , "each state uses its own press action" ~: TestCase $ within $ do
        harness <- newHarness
        executed <- newTQueueIO
        let mark name = RunIO (atomically (writeTQueue executed name))
            pages = [Page "main" [statefulButton "toggle" k0 "one"
                [ state "one" (Static red) (Sequence [SetState "two", mark "one"])
                , state "two" (Static green) (Sequence [SetState "one", mark "two"]) ]]]
        withRuntime harness pages $ do
            _ <- ready harness
            press harness k0
            _ <- ready harness
            assertImage harness k0 green
            assertEqual "first state's action" "one" =<< atomically (readTQueue executed)
            press harness k0
            _ <- ready harness
            assertImage harness k0 red
            assertEqual "second state's action" "two" =<< atomically (readTQueue executed)
    , "old report cannot activate a new page button" ~: TestCase $ within $ do
        harness <- newHarness
        triggered <- newIORef (0 :: Int)
        barrier <- newEmptyMVar
        let pages =
                [ Page "main" [button "next" k0 red (SwitchPage "other")]
                , Page "other"
                    [ button "city" k1 green (RunIO (atomicModifyIORef' triggered (\n -> (n + 1, ()))))
                    , button "barrier" k2 blue (RunIO (putMVar barrier ())) ] ]
        withRuntime harness pages $ do
            _ <- ready harness
            send harness [Pressed k0, Pressed k1]
            _ <- ready harness
            press harness k2
            takeMVar barrier
            assertEqual "second old-layout press ignored" 0 =<< readIORef triggered
            press harness k1
            press harness k2
            takeMVar barrier
            assertEqual "fresh press works" 1 =<< readIORef triggered
    , "input captured during repaint is discarded" ~: TestCase $ within $ do
        harness <- newHarness
        painting <- newEmptyMVar
        release <- newEmptyMVar
        triggered <- newIORef False
        barrier <- newEmptyMVar
        let base = driver harness
            backend = base { drawPageKey = \key image -> do
                when (key == k2 && imageBytes image == imageBytes blue) $
                    putMVar painting () >> takeMVar release
                drawPageKey base key image }
            pages = [Page "main" [button "next" k0 red (SwitchPage "other")],
                     Page "other" [button "city" k1 green (RunIO (atomicModifyIORef' triggered (\_ -> (True, ())))),
                                   button "blue" k2 blue (RunIO (pure ())),
                                   button "barrier" k14 yellow (RunIO (putMVar barrier ()))]]
        config <- right (compilePages "main" pages)
        withAsync (runPageRuntime backend (notify harness) config) $ \worker -> do
            link worker
            _ <- ready harness
            press harness k0
            takeMVar painting
            send harness [Pressed k1, Released k1]
            putMVar release ()
            _ <- ready harness
            press harness k14
            takeMVar barrier
            assertEqual "transition press discarded or flushed" False =<< readIORef triggered
    , "dynamic images refresh and unchanged JPEGs are not resent" ~: TestCase $ within $ do
        harness <- newHarness
        calls <- newIORef (0 :: Int)
        let render = do
                count <- atomicModifyIORef' calls (\n -> (n + 1, n + 1))
                pure (if count <= 3 then green else blue)
            pages = [Page "main" [statefulButton "dynamic" k0 "live"
                [state "live" (refreshEvery 0.005 render) (RunIO (pure ()))]]]
        withRuntime harness pages $ do
            _ <- ready harness
            awaitImage harness k0 blue
            written <- readTVarIO (drawLog harness)
            let copies image = length [() | (key, bytes) <- written, key == k0, bytes == imageBytes image]
            assertEqual "same image sent only once" 1 (copies green)
            assertEqual "new image sent once" 1 (copies blue)
            assertEqual "refresh does not activate another view" Nothing . fmap currentPage =<<
                atomically (tryReadTQueue (activations harness))
    , "renderer failure retries without breaking the page" ~: TestCase $ within $ do
        harness <- newHarness
        calls <- newIORef (0 :: Int)
        let render = do
                count <- atomicModifyIORef' calls (\n -> (n + 1, n + 1))
                case count of
                    1 -> pure green
                    2 -> ioError (userError "expected appearance failure")
                    _ -> pure blue
            pages = [Page "main" [statefulButton "retry" k0 "live"
                [state "live" (refreshEvery 0.005 render) (RunIO (pure ()))]]]
        withRuntime harness pages $ do
            _ <- ready harness
            awaitImage harness k0 green
            awaitImage harness k0 blue
    , "inactive renderers pause and active renderers cancel on state change" ~: TestCase $ within $ do
        harness <- newHarness
        started <- newEmptyMVar
        stopped <- newEmptyMVar
        never <- newEmptyMVar
        hiddenStarted <- newEmptyMVar
        let render = (putMVar started () >> takeMVar never) `finally` putMVar stopped ()
            pages =
                [ Page "main" [statefulButton "clock" k0 "dynamic"
                    [state "dynamic" (refreshEvery 1 render) NextState,
                     state "fixed" (Static green) NextState]]
                , Page "hidden" [statefulButton "hidden" k1 "live"
                    [state "live" (refreshEvery 1 (putMVar hiddenStarted () >> pure red)) NextState]] ]
        withRuntime harness pages $ do
            _ <- ready harness
            takeMVar started
            assertEqual "hidden page has no renderer" Nothing =<< tryTakeMVar hiddenStarted
            press harness k0
            changed <- ready harness
            takeMVar stopped
            assertEqual "fixed state selected" (Just "fixed") (selectedState changed "main" "clock")
            assertImage harness k0 green
    , "renderers stop when leaving a page and restart when returning" ~: TestCase $ within $ do
        harness <- newHarness
        started <- newEmptyMVar
        stopped <- newEmptyMVar
        never <- newEmptyMVar
        let render = (putMVar started () >> takeMVar never) `finally` putMVar stopped ()
            pages = [Page "main" [statefulButton "clock" k0 "live"
                        [state "live" (refreshEvery 1 render) NextState],
                        button "next" k1 green (SwitchPage "other")],
                     Page "other" [button "back" k14 blue (SwitchPage "main")]]
        withRuntime harness pages $ do
            _ <- ready harness
            takeMVar started
            press harness k1
            _ <- ready harness
            takeMVar stopped
            press harness k14
            returned <- ready harness
            assertEqual "selected dynamic state remembered" (Just "live") (selectedState returned "main" "clock")
            takeMVar started
        takeMVar stopped
    ]

configurationValidation :: IO ()
configurationValidation = do
    let good = button "good" k0 red (RunIO (pure ()))
        view action = statefulButton "clock" k0 "one" [state "one" (Static red) action]
        invalid =
            [ [], [Page "other" []], [Page "main" [], Page "main" []]
            , [Page "main" [good, good]]
            , [Page "main" [good, button "other" k0 blue NextState]]
            , [Page "main" [statefulButton "clock" k0 "one" []]]
            , [Page "main" [statefulButton "clock" k0 "missing" [state "one" (Static red) NextState]]]
            , [Page "main" [statefulButton "clock" k0 "one" [state "one" (Static red) NextState, state "one" (Static blue) NextState]]]
            , [Page "main" [view (SwitchPage "missing")]]
            , [Page "main" [view (SetState "missing")]]
            , [Page "main" [view (Sequence [RunIO (pure ()), SwitchPage "missing"])]]
            , [Page "main" [view (Execute "" [])]]
            , [Page "main" [view (Execute "echo" ["bad\0argument"])]]
            , [Page "main" [view (SendKeys (KeyChord [] "bad chord"))]]
            , [Page "main" [button " " k0 red NextState]]
            ]
    forM_ invalid $ \pages -> assertBool "invalid graph rejected" (isLeft (compilePages "main" pages))
    forM_ [0, -1, 0/0, 1/0, 1e30] $ \interval ->
        assertBool "invalid refresh interval rejected" $ isLeft $ compilePages "main"
            [Page "main" [statefulButton "clock" k0 "one"
                [state "one" (refreshEvery interval (pure red)) NextState]]]
    let hiddenInvalid = [Page "main" [statefulButton "clock" k0 "one"
            [state "one" (Static red) NextState, state "two" (Static blue) (SwitchPage "missing")]]]
    outcome <- try (runPages "main" hiddenInvalid) :: IO (Either IOException ())
    assertBool "invalid inactive state fails before opening hardware" (isLeft outcome)

rememberedStates :: IO ()
rememberedStates = do
    config <- right (compilePages "main" rememberedLayout)
    let initial = initialDeckState config
        step key = fst . pressKey config key
        two = step k0 initial
        weather = step k1 two
        weatherChanged = step k0 weather
        returned = step k14 weatherChanged
        three = step k0 returned
        one = step k0 three
    assertEqual "main state remembered" (Just "two") (selectedState returned "main" "clock")
    assertEqual "same button ID is page scoped" (Just "cold") (selectedState returned "weather" "clock")
    assertEqual "next" (Just "three") (selectedState three "main" "clock")
    assertEqual "wrap" (Just "one") (selectedState one "main" "clock")
    assertEqual "unbound press is a no-op" initial (step k14 initial)
    assertEqual "return destination" "main" (currentPage returned)

sequenceActions :: IO ()
sequenceActions = do
    logRef <- newIORef ([] :: [Int])
    let record n = RunIO (atomicModifyIORef' logRef (\values -> (values ++ [n], ())))
        action = Sequence [record 1, SwitchPage "other", NextState, Sequence [record 2, SetState "three"]]
        pages = [Page "main" [statefulButton "clock" k0 "one"
                    [state "one" (Static red) action, state "two" (Static green) NextState,
                     state "three" (Static blue) NextState]], Page "other" []]
    config <- right (compilePages "main" pages)
    let (next, effects) = pressKey config k0 (initialDeckState config)
    assertEqual "page switched" "other" (currentPage next)
    assertEqual "state action targets origin" (Just "three") (selectedState next "main" "clock")
    assertEqual "effects not run by reducer" [] =<< readIORef logRef
    mapM_ performEffect effects
    assertEqual "nested effects preserve order" [1, 2] =<< readIORef logRef

rememberedLayout :: [Page]
rememberedLayout =
    [ Page "main"
        [ statefulButton "clock" k0 "one"
            [ state "one" (Static red) NextState, state "two" (Static green) NextState,
              state "three" (Static blue) NextState ]
        , button "weather" k1 yellow (SwitchPage "weather") ]
    , Page "weather"
        [ statefulButton "clock" k0 "warm"
            [state "warm" (Static yellow) NextState, state "cold" (Static blue) NextState]
        , button "back" k14 blue (SwitchPage "main") ] ]

data Harness = Harness
    { driver :: PageDriver
    , input :: TQueue [KeyEvent]
    , activations :: TQueue DeckState
    , draws :: TQueue (Key, BS.ByteString)
    , screen :: TVar (Map.Map Key BS.ByteString)
    , drawLog :: TVar [(Key, BS.ByteString)]
    , flushCount :: TVar Int
    }

newHarness :: IO Harness
newHarness = do
    incoming <- newTQueueIO
    views <- newTQueueIO
    images <- newTQueueIO
    pixels <- newTVarIO Map.empty
    logVar <- newTVarIO []
    flushes <- newTVarIO 0
    let poll = do
            expired <- registerDelay 10000
            atomically $ readTQueue incoming `orElse` (readTVar expired >>= check >> pure [])
        flush = atomically (modifyTVar' flushes (+ 1) >> drain incoming)
        draw key image = atomically $ do
            let bytes = imageBytes image
            modifyTVar' pixels (Map.insert key bytes)
            modifyTVar' logVar (++ [(key, bytes)])
            writeTQueue images (key, bytes)
    pure (Harness (PageDriver poll flush draw) incoming views images pixels logVar flushes)
  where
    drain queue = tryReadTQueue queue >>= maybe (pure ()) (const (drain queue))

notify :: Harness -> DeckState -> IO ()
notify harness model = atomically (writeTQueue (activations harness) model)

withRuntime :: Harness -> [Page] -> IO a -> IO a
withRuntime harness pages use = do
    config <- right (compilePages "main" pages)
    withAsync (runPageRuntime (driver harness) (notify harness) config) $ \worker -> link worker >> use

ready :: Harness -> IO DeckState
ready = atomically . readTQueue . activations

send :: Harness -> [KeyEvent] -> IO ()
send harness changes = atomically (writeTQueue (input harness) changes)

press :: Harness -> Key -> IO ()
press harness key = send harness [Pressed key, Released key]

assertImage :: Harness -> Key -> KeyImage -> IO ()
assertImage harness key image = do
    pixels <- readTVarIO (screen harness)
    assertEqual "displayed key image" (Just (imageBytes image)) (Map.lookup key pixels)

awaitImage :: Harness -> Key -> KeyImage -> IO ()
awaitImage harness key image = do
    (drawnKey, bytes) <- atomically (readTQueue (draws harness))
    unless (drawnKey == key && bytes == imageBytes image) (awaitImage harness key image)

right :: Either String a -> IO a
right = either (ioError . userError) pure

within :: IO () -> IO ()
within operation = do
    outcome <- timeout 10000000 operation
    case outcome of
        Nothing -> assertFailure "Page runtime test timed out"
        Just () -> pure ()

k0, k1, k2, k14 :: Key
k0 = knownKey 0 0
k1 = knownKey 0 1
k2 = knownKey 0 2
k14 = knownKey 2 4

knownKey :: Int -> Int -> Key
knownKey row column = case keyAt row column of
    Right key -> key
    Left err -> error err

red, green, blue, yellow, black :: KeyImage
red = solidColor 230 20 20
green = solidColor 20 220 20
blue = solidColor 20 20 230
yellow = solidColor 230 220 20
black = solidColor 0 0 0
