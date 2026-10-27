module BatchSpec (tests) where

import Control.Concurrent (newEmptyMVar, newMVar, putMVar, takeMVar, tryTakeMVar, withMVar, yield)
import Control.Concurrent.Async (asyncThreadId, wait, withAsync)
import Control.Exception (IOException, try)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import GHC.Conc (BlockReason (BlockedOnMVar), ThreadStatus (ThreadBlocked), threadStatus)
import StreamDeck.Image (prepareImageReports, solidColor)
import StreamDeck.Internal.Batch (writeBatch)
import StreamDeck.Protocol (keyAt)
import System.Timeout (timeout)
import Test.HUnit (Test (..), (~:), assertBool, assertEqual, assertFailure)

tests :: Test
tests = TestList
    [ "blocking reads cannot interleave between batch packets" ~: TestCase $ within $ do
        lock <- newMVar ()
        first <- newEmptyMVar
        trying <- newEmptyMVar
        logRef <- newIORef ([] :: [String])
        let record entry = atomicModifyIORef' logRef (\entries -> (entries ++ [entry], ()))
            reader = takeMVar first >> putMVar trying () >> withMVar lock (\_ -> record "read")
        withAsync reader $ \worker -> do
            let queued = do
                    status <- threadStatus (asyncThreadId worker)
                    case status of
                        ThreadBlocked BlockedOnMVar -> pure ()
                        _ -> yield >> queued
                send packet = do
                    record (show (BS.unpack packet))
                    if packet == BS.singleton 1
                        then putMVar first () >> takeMVar trying >> queued
                        else pure ()
            writeBatch lock send (map BS.singleton [1, 2, 3])
            wait worker
        assertEqual "one uninterrupted burst before queued reader" ["[1]", "[2]", "[3]", "read"] =<< readIORef logRef
    , "all batch reports are validated before the first write" ~: TestCase $ within $ do
        lock <- newMVar ()
        count <- newIORef (0 :: Int)
        result <- try (writeBatch lock (\_ -> atomicModifyIORef' count (\n -> (n + 1, ())))
            [BS.singleton 1, BS.empty]) :: IO (Either IOException ())
        assertBool "invalid report rejected" (isLeft result)
        assertEqual "no partial burst" 0 =<< readIORef count
        assertEqual "lock remains available" (Just ()) =<< tryTakeMVar lock
    , "failed batch writes release the transport lock" ~: TestCase $ within $ do
        lock <- newMVar ()
        let send packet = unless (packet == BS.singleton 1) (ioError (userError "expected native write failure"))
        result <- try (writeBatch lock send (map BS.singleton [1, 2, 3])) :: IO (Either IOException ())
        assertBool "write failure propagated" (isLeft result)
        withMVar lock (\_ -> pure ())
    , "empty batches do not wait for a busy reader" ~: TestCase $ within $ do
        busy <- newEmptyMVar
        writeBatch busy (\_ -> assertFailure "Unexpected empty-batch write") []
    , "cached image packets retain the correct target key" ~: TestCase $ do
        first <- either fail pure (keyAt 0 0)
        lastKey <- either fail pure (keyAt 2 4)
        let image = solidColor 20 180 70
        firstReports <- prepareImageReports first image
        lastReports <- prepareImageReports lastKey image
        repeated <- prepareImageReports first image
        assertEqual "same key's packet stream reused unchanged" firstReports repeated
        assertBool "first key header" (all ((== 0) . (`BS.index` 2)) firstReports)
        assertBool "last key header" (all ((== 14) . (`BS.index` 2)) lastReports)
        assertEqual "only key index differs" (map clearKey firstReports) (map clearKey lastReports)
    ]
  where
    clearKey packet = BS.take 2 packet <> BS.singleton 0 <> BS.drop 3 packet

within :: IO () -> IO ()
within operation = do
    result <- timeout 5000000 operation
    case result of
        Nothing -> assertFailure "Batch scheduling test timed out"
        Just () -> pure ()
