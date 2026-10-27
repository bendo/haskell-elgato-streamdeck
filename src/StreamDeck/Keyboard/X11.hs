{-# LANGUAGE ForeignFunctionInterface #-}

-- | Native XTest input. The typed public interface is 'StreamDeck.Action.SendKeys'.
module StreamDeck.Keyboard.X11 (sendKeyNames, sendKeyNamesTo) where

import Control.Monad (unless)
import Foreign (Ptr, allocaBytes, nullPtr, withArray)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Utils (withMany)

foreign import ccall safe "streamdeck_send_keys"
    nativeSend :: CString -> Ptr CString -> CSize -> CString -> CSize -> IO CInt

-- | Use DISPLAY from the environment. All native calls and complete chords are serialized.
sendKeyNames :: [String] -> IO ()
sendKeyNames = sendKeyNamesTo Nothing

-- | Explicit display selection, also useful for isolated X-server tests.
sendKeyNamesTo :: Maybe String -> [String] -> IO ()
sendKeyNamesTo display names =
    withDisplay $ \displayName -> withMany withCString names $ \strings ->
        withArray strings $ \keys -> allocaBytes 512 $ \message -> do
            result <- nativeSend displayName keys (fromIntegral (length names)) message 512
            unless (result /= 0) $ do
                reason <- peekCString message
                ioError (userError ("SendKeys: " ++ reason))
  where
    withDisplay operation = maybe (operation nullPtr) (`withCString` operation) display
