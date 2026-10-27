{-# LANGUAGE ForeignFunctionInterface #-}

-- | Narrow native I/O wrapper around the HIDAPI implementation linked by hidapi.
-- hidapi-0.1.8's Haskell error wrapper incorrectly frees hid_error's borrowed
-- string. Keep native handles here and copy error messages without freeing them.
module StreamDeck.Transport
    ( Device, openPath, close, readTimeout, write, getFeatureReport
    ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (throwIO)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Foreign (Ptr, allocaBytes, castPtr, nullPtr, pokeByteOff)
import Foreign.C.String (CString, CWString, peekCWString, withCString)
import Foreign.C.Types (CInt (..), CSize (..))
import System.HIDAPI (HIDAPIException (..))

data NativeDevice
data Device = Device (Ptr NativeDevice) (MVar ())

foreign import ccall safe "hid_open_path"
    nativeOpen :: CString -> IO (Ptr NativeDevice)
foreign import ccall safe "hid_close"
    nativeClose :: Ptr NativeDevice -> IO ()
foreign import ccall safe "streamdeck_read_timeout"
    nativeRead :: Ptr NativeDevice -> Ptr Word8 -> CSize -> CInt -> IO CInt
foreign import ccall safe "hid_write"
    nativeWrite :: Ptr NativeDevice -> Ptr Word8 -> CSize -> IO CInt
foreign import ccall safe "hid_get_feature_report"
    nativeFeature :: Ptr NativeDevice -> Ptr Word8 -> CSize -> IO CInt
foreign import ccall safe "hid_error"
    nativeError :: Ptr NativeDevice -> IO CWString

openPath :: FilePath -> IO Device
openPath path = do
    lock <- newMVar ()
    pointer <- withCString path nativeOpen
    -- The bundled backend stores open errors in thread-local storage; a later
    -- safe FFI call need not use the same OS thread. Use a stable fallback here.
    unless (pointer /= nullPtr) $
        throwIO (HIDAPIException "Device open failed" ("Unable to open " ++ path))
    pure (Device pointer lock)

close :: Device -> IO ()
close (Device pointer lock) = withMVar lock $ \_ -> nativeClose pointer

readTimeout :: Device -> Int -> Int -> IO BS.ByteString
readTimeout (Device pointer lock) size milliseconds = do
    validateSize size
    unless (milliseconds >= 0 && toInteger milliseconds <= 2147483647) $
        ioError (userError "Invalid HID read timeout")
    withMVar lock $ \_ -> allocaBytes size $ \buffer -> do
        count <- nativeRead pointer buffer (fromIntegral size) (fromIntegral milliseconds)
        checkCount pointer "Read failed" size count
        BS.packCStringLen (castPtr buffer, fromIntegral count)

write :: Device -> BS.ByteString -> IO Int
write (Device pointer lock) bytes = do
    validateSize (BS.length bytes)
    withMVar lock $ \_ -> BS.useAsCStringLen bytes $ \(buffer, size) -> do
        count <- nativeWrite pointer (castPtr buffer) (fromIntegral size)
        checkCount pointer "Write failed" size count
        pure (fromIntegral count)

-- | Length includes the report ID, as in the native HIDAPI API.
getFeatureReport :: Device -> Word8 -> Int -> IO (Word8, BS.ByteString)
getFeatureReport (Device pointer lock) reportId size = do
    validateSize size
    withMVar lock $ \_ -> allocaBytes size $ \buffer -> do
        pokeByteOff buffer 0 reportId
        count <- nativeFeature pointer buffer (fromIntegral size)
        checkCount pointer "Feature report read failed" size count
        bytes <- BS.packCStringLen (castPtr buffer, fromIntegral count)
        case BS.uncons bytes of
            Nothing -> throwIO (HIDAPIException "Feature report read failed" "Empty response")
            Just result -> pure result

checkCount :: Ptr NativeDevice -> String -> Int -> CInt -> IO ()
checkCount pointer operation limit count
    | count < 0 = do
        message <- nativeError pointer
        reason <- if message == nullPtr then pure "Native HIDAPI error" else peekCWString message
        -- The pointer is owned by HIDAPI, including when it points to a literal.
        throwIO (HIDAPIException operation reason)
    | fromIntegral count > limit =
        throwIO (HIDAPIException operation "Unexpected native report length")
    | otherwise = pure ()

validateSize :: Int -> IO ()
validateSize size = unless (size > 0 && toInteger size <= 2147483647) $
    ioError (userError "HID report size must be within 1..2147483647 bytes")
