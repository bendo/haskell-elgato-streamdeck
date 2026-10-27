{-# LANGUAGE ForeignFunctionInterface #-}

module KeyboardSpec (tests) where

import Control.Concurrent.Async (concurrently_, withAsync)
import Control.Exception (IOException, bracket, try)
import Control.Monad (forM_, forever, replicateM, unless)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Data.List (isInfixOf)
import Foreign (Ptr, alloca, allocaArray, allocaBytes, nullPtr, peek, peekArray)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..), CUInt (..), CULong (..))
import StreamDeck.Action
import StreamDeck.Keyboard.X11 (sendKeyNamesTo)
import System.Directory (findExecutable)
import System.Environment (lookupEnv)
import Test.HUnit (Test (..), (~:), assertBool, assertEqual, assertFailure)

data Fixture
data KeyEvent = KeyEvent Bool CULong CUInt deriving (Eq, Show)

foreign import ccall safe "streamdeck_test_x11_open" openFixture :: IO (Ptr Fixture)
foreign import ccall safe "streamdeck_test_x11_close" closeFixture :: Ptr Fixture -> IO ()
foreign import ccall safe "streamdeck_test_x11_drain" drain :: Ptr Fixture -> IO ()
foreign import ccall safe "streamdeck_test_x11_next"
    nextEvent :: Ptr Fixture -> Ptr CUInt -> Ptr CULong -> Ptr CUInt -> CInt -> IO CInt
foreign import ccall safe "streamdeck_test_x11_symbol" symbol :: CString -> IO CULong
foreign import ccall safe "streamdeck_test_x11_hold" holdKey :: Ptr Fixture -> CString -> CInt -> IO CInt
foreign import ccall safe "streamdeck_test_x11_keys" keys :: Ptr Fixture -> CString -> IO ()
foreign import ccall safe "streamdeck_test_x11_state" keyboardState :: Ptr Fixture -> Ptr CUInt -> IO ()
foreign import ccall safe "streamdeck_test_x11_lock" lockModifiers :: Ptr Fixture -> CUInt -> CUInt -> IO ()
foreign import ccall safe "streamdeck_test_x11_group" lockGroup :: Ptr Fixture -> CUInt -> IO ()
foreign import ccall safe "streamdeck_test_x11_unmap" unmapKey :: Ptr Fixture -> CString -> IO ()

-- Never generate input on the user's desktop in the ordinary test suite.
tests :: IO Test
tests = do
    mode <- lookupEnv "STREAMDECK_TEST_X11"
    layoutTool <- findExecutable "setxkbmap"
    pure $ TestList $
        [ "native keyboard display errors" ~: TestCase $ do
            expectFailure "DISPLAY" (sendKeyNamesTo (Just "") ["Super_L", "d"])
            expectFailure "Cannot open X11 display"
                (sendKeyNamesTo (Just "streamdeck-invalid-display") ["Super_L", "d"])
        ] ++ case mode of
            Just "1" -> integrationTests layoutTool
            Just "no-xtest" ->
                [ "missing XTEST extension" ~: TestCase $
                    expectFailure "XTEST" (performEffect (SendKeys (super 'd'))) ]
            _ -> []

integrationTests :: Maybe FilePath -> [Test]
integrationTests layoutTool =
    [ "native Super+d and Super+1 event order" ~: TestCase $ withFixture $ \fixture -> do
        superSym <- named "Super_L"
        forM_ ['d', '1'] $ \key -> do
            target <- named [key]
            performEffect (SendKeys (super key))
            events <- replicateM 4 (receive fixture)
            assertEqual "press modifiers, press/release target, release modifiers"
                [(True, superSym), (True, target), (False, target), (False, superSym)] (map eventIdentity events)
            let KeyEvent _ _ mask = events !! 1
            assertBool "Super modifier on target press" (mask .&. 64 /= 0)
            assertReleased fixture
    , "native held modifiers and locks are restored" ~: TestCase $ withFixture $ \fixture -> do
        hold fixture "Shift_L" True
        hold fixture "Control_L" True
        hold fixture "Alt_L" True
        hold fixture "Super_L" True
        lockModifiers fixture 2 0 -- Caps Lock
        beforeKeys <- snapshot fixture
        beforeState <- stateSnapshot fixture
        drain fixture
        performEffect (SendKeys (super 'd'))
        events <- replicateM 12 (receive fixture)
        d <- named "d"
        let targetMasks = [mask | KeyEvent True key mask <- events, key == d]
        assertEqual "unrelated held modifiers and Caps Lock cleared during chord" [64] targetMasks
        assertEqual "all physically held modifiers restored" beforeKeys =<< snapshot fixture
        assertEqual "locked modifiers restored" beforeState =<< stateSnapshot fixture
    , "native latched modifiers are restored" ~: TestCase $ withFixture $ \fixture -> do
        lockModifiers fixture 0 1 -- latched Shift
        before <- stateSnapshot fixture
        drain fixture
        performEffect (SendKeys (super 'd'))
        events <- replicateM 4 (receive fixture)
        let KeyEvent _ _ mask = events !! 1
        assertEqual "latched Shift cleared during chord" 64 (mask .&. 255)
        assertEqual "latched state restored" before =<< stateSnapshot fixture
        assertReleased fixture
    , "native shifted symbols add Shift" ~: TestCase $ withFixture $ \fixture -> do
        performEffect (SendKeys (KeyChord [Super] "exclam"))
        events <- replicateM 6 (receive fixture)
        exclam <- named "exclam"
        assertEqual "Shift required for exclam" [65]
            [mask .&. 255 | KeyEvent True key mask <- events, key == exclam]
        assertReleased fixture
    , "native invalid and unmapped keys generate no input" ~: TestCase $ withFixture $ \fixture -> do
        hold fixture "Shift_L" True
        lockModifiers fixture 2 0
        beforeKeys <- snapshot fixture
        beforeState <- stateSnapshot fixture
        drain fixture
        expectFailure "Unknown X11 keysym" (performEffect (SendKeys (KeyChord [] "StreamDeckNotAKeysym")))
        withCString "F12" (unmapKey fixture)
        drain fixture
        expectFailure "not mapped" (performEffect (SendKeys (KeyChord [] "F12")))
        assertEqual "validation does not change held keys" beforeKeys =<< snapshot fixture
        assertEqual "validation does not change locks" beforeState =<< stateSnapshot fixture
        assertEqual "validation generates no key events" Nothing =<< receiveMaybe fixture 30
    , "native already-held target is not released" ~: TestCase $ withFixture $ \fixture -> do
        hold fixture "d" True
        before <- snapshot fixture
        drain fixture
        expectFailure "already held" (performEffect (SendKeys (super 'd')))
        assertEqual "target remains held" before =<< snapshot fixture
        assertEqual "no events generated" Nothing =<< receiveMaybe fixture 30
    , "native concurrent chords do not interleave" ~: TestCase $ withFixture $ \fixture -> do
        concurrently_ (performEffect (SendKeys (super 'd')))
            (performEffect (SendKeys (KeyChord [Control] "1")))
        events <- map eventIdentity <$> replicateM 8 (receive fixture)
        superSym <- named "Super_L"
        control <- named "Control_L"
        d <- named "d"
        one <- named "1"
        let a = [(True, superSym), (True, d), (False, d), (False, superSym)]
            b = [(True, control), (True, one), (False, one), (False, control)]
        assertBool "whole chords serialized" (events == a ++ b || events == b ++ a)
        assertReleased fixture
    , "native cancellation leaves no injected keys held" ~: TestCase $ withFixture $ \fixture -> do
        withAsync (forever (performEffect (SendKeys (super 'd')))) $ \_ -> do
            _ <- receive fixture
            pure ()
        assertReleased fixture
    ] ++ case layoutTool of
        Nothing -> []
        Just program ->
            [ "native active keyboard group is preserved" ~: TestCase $ withFixture $ \fixture -> do
                performEffect (Execute program ["-layout", "us,cz"])
                drain fixture
                lockGroup fixture 1
                before <- stateSnapshot fixture
                assertEqual "second layout selected" 1 (last before)
                performEffect (SendKeys (super 'd'))
                events <- replicateM 4 (receive fixture)
                d <- named "d"
                assertBool "key resolved in active layout" (any ((== (True, d)) . eventIdentity) events)
                assertEqual "layout preserved" before =<< stateSnapshot fixture
                assertReleased fixture
            ]

withFixture :: (Ptr Fixture -> IO a) -> IO a
withFixture = bracket acquire closeFixture
  where
    acquire = do
        fixture <- openFixture
        unless (fixture /= nullPtr) $ ioError (userError "Cannot open private X11 test display")
        drain fixture
        pure fixture

hold :: Ptr Fixture -> String -> Bool -> IO ()
hold fixture key down = withCString key $ \name -> do
    result <- holdKey fixture name (if down then 1 else 0)
    unless (result /= 0) (ioError (userError ("Cannot hold fixture key " ++ key)))

named :: String -> IO CULong
named name = withCString name symbol

snapshot :: Ptr Fixture -> IO BS.ByteString
snapshot fixture = allocaBytes 32 $ \buffer -> keys fixture buffer >> BS.packCStringLen (buffer, 32)

stateSnapshot :: Ptr Fixture -> IO [CUInt]
stateSnapshot fixture = allocaArray 5 $ \buffer -> keyboardState fixture buffer >> peekArray 5 buffer

assertReleased :: Ptr Fixture -> IO ()
assertReleased fixture = assertEqual "no injected keys remain down" (BS.replicate 32 0) =<< snapshot fixture

receive :: Ptr Fixture -> IO KeyEvent
receive fixture = do
    event <- receiveMaybe fixture 1000
    maybe (ioError (userError "Timed out waiting for injected X11 event")) pure event

receiveMaybe :: Ptr Fixture -> Int -> IO (Maybe KeyEvent)
receiveMaybe fixture milliseconds = alloca $ \pressed -> alloca $ \key -> alloca $ \mask -> do
    result <- nextEvent fixture pressed key mask (fromIntegral milliseconds)
    if result == 0 then pure Nothing else if result < 0
        then ioError (userError "X11 event fixture poll failed")
        else do
            down <- peek pressed
            Just <$> (KeyEvent (down /= 0) <$> peek key <*> peek mask)

eventIdentity :: KeyEvent -> (Bool, CULong)
eventIdentity (KeyEvent pressed key _) = (pressed, key)

expectFailure :: String -> IO () -> IO ()
expectFailure fragment operation = do
    result <- try operation :: IO (Either IOException ())
    case result of
        Left err -> assertBool ("Expected error containing " ++ show fragment ++ ", got " ++ show err)
            (fragment `isInfixOf` show err)
        Right () -> assertFailure ("Expected failure containing " ++ fragment)
