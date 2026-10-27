module Main where

import Codec.Picture
    ( PixelRGB8 (..), convertRGB8, decodeJpeg, generateImage
    , imageWidth, imageHeight, pixelAt )
import Control.Concurrent (newChan, readChan, writeChan, newEmptyMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Concurrent.Async (withAsync)
import Control.Exception (IOException, bracket, finally, try)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.Word (Word8)
import DeviceManager (withHIDAPI)
import qualified FontSpec
import qualified KeyboardSpec
import qualified PageSpec
import StreamDeck (Button (..), runDeck, solidColor)
import StreamDeck.Image (imageBytes, prepareKeyImage)
import StreamDeck.Protocol
import StreamDeck.Runtime (runEventLoop)
import qualified StreamDeck.Transport as Transport
import System.Exit (exitFailure)
import System.HIDAPI (HIDAPIException)
import System.Info (os)
import System.Timeout (timeout)
import Test.HUnit

main :: IO ()
main = do
    keyboardTests <- KeyboardSpec.tests
    result <- runTestTT (TestList [tests, keyboardTests])
    if errors result + failures result == 0 then pure () else exitFailure

tests :: Test
tests = TestList
    [ FontSpec.tests
    , PageSpec.tests
    , "key positions" ~: TestCase $ do
        forM_ [(row, column) | row <- [0..2], column <- [0..4]] $ \position@(row, column) -> do
            key <- right (keyAt row column)
            assertEqual "position round trip" position (keyPosition key)
            assertEqual "row-major index" (row * 5 + column) (keyIndex key)
        forM_ [(-1, 0), (0, -1), (3, 0), (0, 5)] $ \(row, column) ->
            assertBool "reject invalid position" (isLeft (keyAt row column))
    , "key transitions" ~: TestCase $ do
        first <- right (keyAt 0 0)
        lastKey <- right (keyAt 2 4)
        down <- right (decodeKeyReport (report [0, 14]))
        one <- right (decodeKeyReport (report [14]))
        assertEqual "simultaneous press" [Pressed first, Pressed lastKey]
            (keyEvents idleKeyState down)
        assertEqual "held keys do not repeat" [] (keyEvents down down)
        assertEqual "one release" [Released first] (keyEvents down one)
        assertEqual "last release" [Released lastKey] (keyEvents one idleKeyState)
        assertEqual "padding accepted" (Right down)
            (decodeKeyReport (report [0, 14] <> BS.replicate 13 0))
    , "malformed input" ~: TestCase $ do
        forM_ [BS.empty, BS.take 18 (report []), BS.cons 2 (BS.drop 1 (report [])),
               BS.take 4 (report []) <> BS.singleton 2 <> BS.replicate 14 0] $ \bytes ->
            assertBool "reject malformed report" (isLeft (decodeKeyReport bytes))
    , "image packet boundaries" ~: TestCase $ do
        key <- right (keyAt 2 4)
        assertBool "reject empty image" (isLeft (imageReports key BS.empty))
        forM_ [1, 1015, 1016, 1017, 2032, 2033, 1016 * 256 + 1] $ \size -> do
            let payload = BS.pack (take size (cycle [0..255]))
            packets <- right (imageReports key payload)
            assertEqual "page count" ((size + 1015) `div` 1016) (length packets)
            forM_ (zip [0 :: Int ..] packets) $ \(page, packet) -> do
                let remaining = size - page * 1016
                    chunkSize = min remaining 1016
                assertEqual "fixed report length" 1024 (BS.length packet)
                assertEqual "header" (BS.pack
                    [2, 7, 14, if remaining <= 1016 then 1 else 0,
                     fromIntegral chunkSize, fromIntegral (chunkSize `div` 256),
                     fromIntegral page, fromIntegral (page `div` 256)]) (BS.take 8 packet)
                assertEqual "zero padding" (BS.replicate (1016 - chunkSize) 0)
                    (BS.drop (8 + chunkSize) packet)
            let contents packet =
                    let sizeLow = fromIntegral (BS.index packet 4)
                        sizeHigh = fromIntegral (BS.index packet 5)
                    in BS.take (sizeLow + 256 * sizeHigh) (BS.drop 8 packet)
            assertEqual "payload survives packetization" payload (BS.concat (map contents packets))
    , "image size and orientation" ~: TestCase $ do
        let source = generateImage (\x y ->
                if x < 40 && y < 40 then PixelRGB8 240 10 10
                else PixelRGB8 10 20 240) 144 144
        image <- right (prepareKeyImage source)
        decoded <- convertRGB8 <$> right (decodeJpeg (imageBytes image))
        assertEqual "width" 72 (imageWidth decoded)
        assertEqual "height" 72 (imageHeight decoded)
        let PixelRGB8 r1 _ b1 = pixelAt decoded 66 66
            PixelRGB8 r2 _ b2 = pixelAt decoded 5 5
        assertBool "upright top-left becomes device bottom-right" (r1 > 200 && b1 < 40)
        assertBool "upright bottom-right becomes device top-left" (b2 > 200 && r2 < 40)
        assertBool "reject zero-sized images"
            (isLeft (prepareKeyImage (generateImage (\_ _ -> PixelRGB8 0 0 0) 0 0)))
        solid <- convertRGB8 <$> right (decodeJpeg (imageBytes (solidColor 0 200 0)))
        assertEqual "solid width" 72 (imageWidth solid)
    , "duplicate bindings fail before opening hardware" ~: TestCase $ do
        key <- right (keyAt 0 0)
        let binding = Button key (solidColor 0 0 0) (pure ())
        outcome <- try (runDeck [binding, binding]) :: IO (Either IOException ())
        assertBool "duplicate key rejected" (isLeft outcome)
    , "actions do not block polling and run in order" ~: TestCase $ within $ do
        first <- right (keyAt 0 0)
        second <- right (keyAt 0 1)
        input <- newChan
        polled <- newChan
        started <- newEmptyMVar
        release <- newEmptyMVar
        completed <- newEmptyMVar
        let poll = do
                events <- readChan input
                writeChan polled ()
                pure events
            bindings =
                [ (first, putMVar started () >> takeMVar release)
                , (second, putMVar completed ())
                ]
        withAsync (runEventLoop poll bindings) $ \_ -> do
            writeChan input [Pressed first]
            readChan polled
            takeMVar started
            writeChan input [Released first, Pressed second]
            readChan polled
            assertEqual "second action waits for first" Nothing =<< tryTakeMVar completed
            putMVar release ()
            takeMVar completed
    , "action failure does not stop later presses" ~: TestCase $ within $ do
        first <- right (keyAt 0 0)
        second <- right (keyAt 0 1)
        input <- newChan
        completed <- newEmptyMVar
        let bindings = [(first, ioError (userError "expected test action failure")),
                        (second, putMVar completed ())]
        withAsync (runEventLoop (readChan input) bindings) $ \_ -> do
            writeChan input [Pressed first, Released second, Pressed second]
            takeMVar completed
    , "cancellation cleans up an active action" ~: TestCase $ within $ do
        key <- right (keyAt 0 0)
        input <- newChan
        started <- newEmptyMVar
        blocked <- newEmptyMVar
        cleaned <- newEmptyMVar
        let action = (putMVar started () >> takeMVar blocked) `finally` putMVar cleaned ()
        withAsync (runEventLoop (readChan input) [(key, action)]) $ \_ -> do
            writeChan input [Pressed key]
            takeMVar started
        takeMVar cleaned
    , "native error strings remain owned by HIDAPI" ~: TestCase $
        if os /= "linux" then pure () else withHIDAPI $
            -- Linux's hidraw backend opens this node, but feature ioctls fail.
            -- Copying and then freeing the borrowed error string would make
            -- nativeClose double-free it, aborting the process.
            bracket (Transport.openPath "/dev/null") Transport.close $ \device -> do
                outcome <- try (Transport.getFeatureReport device 0x05 32)
                    :: IO (Either HIDAPIException (Word8, BS.ByteString))
                assertBool "feature ioctl fails without aborting cleanup" (isLeft outcome)
    ]

right :: Either String a -> IO a
right = either (ioError . userError) pure

report :: [Int] -> BS.ByteString
report down = BS.pack ([1, 0, 15, 0] ++ [if index `elem` down then 1 else 0 | index <- [0..14]])

within :: IO () -> IO ()
within action = do
    outcome <- timeout 5000000 action
    case outcome of
        Just () -> pure ()
        Nothing -> assertFailure "Event loop test timed out"
