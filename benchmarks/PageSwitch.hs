module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (link, withAsync)
import Control.Concurrent.STM (atomically, newTQueueIO, readTQueue, tryReadTQueue, writeTQueue)
import Control.Monad (forM, forM_, forever, unless, void)
import qualified Data.ByteString as BS
import qualified DeviceManager as Device
import GHC.Clock (getMonotonicTimeNSec)
import StreamDeck
import qualified StreamDeck.FontAwesome as FA
import StreamDeck.Image (imageBytes, prepareImageReports)
import StreamDeck.Pages (compilePages, currentPage)
import StreamDeck.Pages.Runtime (PageDriver (..), runPageRuntime)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import System.Timeout (timeout)
import Text.Printf (printf)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    putStrLn "MK2 page benchmark: fills all 15 keys with test icons."
    fonts <- FA.loadFontAwesome
    before <- getMonotonicTimeNSec
    frameA <- makeFrame fonts (RGB 255 255 255)
    frameB <- makeFrame fonts (RGB 60 180 240)
    prepare frameA
    prepare frameB
    prepared <- getMonotonicTimeNSec
    printTime "Cold render/encode/packet preparation (two frames)" before prepared
    measure "Warm cached packet preparation (one frame)" (prepare frameA)
    reports <- concat <$> mapM (uncurry prepareImageReports) frameA
    printf "Frame A: %d keys, %d reports, %d encoded JPEG bytes\n"
        (length frameA) (length reports) (sum [BS.length (imageBytes image) | (_, image) <- frameA])
    Device.withHIDAPI $ do
        devices <- Device.enumerate
        case devices of
            [] -> fail "No Stream Deck MK2 found"
            info : _ -> Device.withDevice info $ \deck -> do
                measure "Batch transfer without reader" (Device.setKeyImages deck frameA)
                forM_ [100, 20] $ \milliseconds ->
                    withAsync (forever (void (Device.readKeyEvents deck milliseconds))) $ \reader -> do
                        link reader
                        threadDelay 10000
                        forM_ [1..3 :: Int] $ \iteration ->
                            measure ("Batch with " ++ show milliseconds ++ " ms reader, run " ++ show iteration)
                                (Device.setKeyImages deck frameA)
                runtimeBenchmark deck frameA frameB

makeFrame :: FA.FontAwesome -> Color -> IO [(Key, KeyImage)]
makeFrame fonts color = forM (zip [0..14] icons) $ \(index, icon) -> do
    key <- either fail pure (keyAt (index `div` 5) (index `mod` 5))
    image <- either fail pure (FA.renderAwesomeIcon fonts defaultIconStyle { foreground = color } icon)
    pure (key, image)
  where
    icons = cycle [FA.spotify, FA.volumeHigh, FA.sun, FA.cloudRain, FA.snowflake]

prepare :: [(Key, KeyImage)] -> IO ()
prepare = mapM_ (void . uncurry prepareImageReports)

runtimeBenchmark :: Device.Deck -> [(Key, KeyImage)] -> [(Key, KeyImage)] -> IO ()
runtimeBenchmark deck frameA frameB = do
    trigger <- either fail pure (keyAt 0 0)
    incoming <- newTQueueIO
    activations <- newTQueueIO
    transfers <- newTQueueIO
    let page name destination images = Page name
            [button (show key) key image (if key == trigger then SwitchPage destination else RunIO (pure ()))
            | (key, image) <- images]
        poll = do
            physical <- Device.readKeyEvents deck 20
            scripted <- atomically (tryReadTQueue incoming)
            pure (physical ++ maybe [] id scripted)
        draw images = do
            before <- getMonotonicTimeNSec
            Device.setKeyImages deck images
            after <- getMonotonicTimeNSec
            atomically (writeTQueue transfers (before, after, length images))
        observe model = do
            now <- getMonotonicTimeNSec
            atomically (writeTQueue activations (currentPage model, now))
    config <- either fail pure (compilePages "a" [page "a" "b" frameA, page "b" "a" frameB])
    outcome <- timeout 10000000 $
        withAsync (runPageRuntime (PageDriver poll (Device.flushKeyEvents deck) draw) observe config) $ \worker -> do
            link worker
            _ <- atomically (readTQueue activations)
            _ <- atomically (readTQueue transfers)
            forM_ (zip [1..3 :: Int] ["b", "a", "b"]) $ \(iteration, expected) -> do
                start <- getMonotonicTimeNSec
                atomically (writeTQueue incoming [Pressed trigger, Released trigger])
                (destination, ready) <- atomically (readTQueue activations)
                unless (destination == expected) (fail "Unexpected page; avoid physical key presses during benchmark")
                (transferStart, transferEnd, count) <- atomically (readTQueue transfers)
                printf "Runtime switch %d: %d changed keys\n" iteration count
                printTime "  input + preparation + handoff" start transferStart
                printTime "  uninterrupted device burst" transferStart transferEnd
                printTime "  total through input flush/activation" start ready
    unless (outcome == Just ()) (fail "Page benchmark timed out")

measure :: String -> IO () -> IO ()
measure label operation = do
    before <- getMonotonicTimeNSec
    operation
    after <- getMonotonicTimeNSec
    printTime label before after

printTime :: (Integral a) => String -> a -> a -> IO ()
printTime label before after = printf "%s: %.3f ms\n" label (fromIntegral (after - before) / 1000000 :: Double)
