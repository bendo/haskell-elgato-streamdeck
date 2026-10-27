-- | Shared scheduling primitive for native writes and transport-independent tests.
module StreamDeck.Internal.Batch (writeBatch) where

import Control.Concurrent.MVar (MVar, withMVar)
import Control.Monad (unless)
import qualified Data.ByteString as BS

-- | Validate and fully prepare all reports before taking the I/O lock. Hold it
-- for the complete burst, so blocking reads cannot interleave between packets.
writeBatch :: MVar () -> (BS.ByteString -> IO ()) -> [BS.ByteString] -> IO ()
writeBatch lock send reports = do
    mapM_ validate reports
    unless (null reports) $ withMVar lock $ \_ -> mapM_ send reports
  where
    validate report = unless (BS.length report > 0 && toInteger (BS.length report) <= 2147483647) $
        ioError (userError "HID report size must be within 1..2147483647 bytes")
