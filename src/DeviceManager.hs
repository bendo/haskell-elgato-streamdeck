module DeviceManager
  ( Deck
  , DeckType (..)
  , DeviceInfo
  , withHIDAPI
  , enumerate
  , withDevice
  , open
  , close
  , serialNumber
  , firmwareVersion
  , readKeyEvents
  , flushKeyEvents
  , setKeyImage
  )
where

import           Control.Concurrent.MVar (MVar, modifyMVar, newMVar, withMVar)
import           Control.Exception (bracket)
import           Control.Monad (forM_, unless)
import qualified Data.ByteString as BS
import qualified Data.Word       as DW
import           System.HIDAPI   (DeviceInfo)
import qualified System.HIDAPI   as HID
import qualified StreamDeck.Transport as Transport
import           StreamDeck.Image (KeyImage, imageBytes)
import           StreamDeck.Protocol
    ( Key, KeyEvent, KeyState, decodeKeyReport, idleKeyState, imageReports, keyEvents )

-- StreamDeckMini 0x0063
-- StreamDeckOriginal 0x0060
-- StreamDeckMK2 0x0080
-- StreamDeckXL 0x006c

data DeckType = StreamDeckMini | StreamDeck | StreamDeckMK2 | StreamDeckXL

data Deck = Deck Transport.Device (MVar KeyState) (MVar ())

vendorId :: DW.Word16
vendorId = 0x0fd9

productId :: DeckType -> HID.ProductID
productId StreamDeckMini = 0x0063
productId StreamDeck     = 0x0060
productId StreamDeckMK2  = 0x0080
productId StreamDeckXL   = 0x006c

-- | Initialize HIDAPI for the complete discovery and device session.
-- All devices must be closed before this action returns.
withHIDAPI :: IO a -> IO a
withHIDAPI = HID.withHIDAPI

-- | Discover MK2 devices. Call within 'withHIDAPI'.
enumerate :: IO [DeviceInfo]
enumerate = HID.enumerate (Just vendorId) (Just $ productId StreamDeckMK2)

-- | Open a device and close it on normal return or an exception.
-- Call within 'withHIDAPI'; the device must not escape the callback.
withDevice :: DeviceInfo -> (Deck -> IO a) -> IO a
withDevice deviceInfo = bracket (open deviceInfo) close

-- | Open a device within 'withHIDAPI'. Prefer 'withDevice' for automatic cleanup.
open :: DeviceInfo -> IO Deck
open deviceInfo = do
    state <- newMVar idleKeyState
    outputLock <- newMVar ()
    device <- Transport.openPath (HID.path deviceInfo)
    pure (Deck device state outputLock)

-- | Close an open device exactly once, before leaving 'withHIDAPI'.
close :: Deck -> IO ()
close (Deck device _ _) = Transport.close device

serialNumber :: DeviceInfo -> Maybe String
serialNumber = HID.serialNumber

-- | Read the raw MK2 firmware feature report (report ID and payload).
firmwareVersion :: Deck -> IO (DW.Word8, BS.ByteString)
firmwareVersion (Deck device _ _) = Transport.getFeatureReport device 0x05 32

-- | Wait up to the supplied milliseconds for a report; return transitions only.
-- A timeout returns no events and preserves the previous state. Reads are serialized.
readKeyEvents :: Deck -> Int -> IO [KeyEvent]
readKeyEvents deck timeout = maybe [] id <$> readKeyReport deck timeout

-- | Drain reports without dispatching actions, while retaining current key state.
flushKeyEvents :: Deck -> IO ()
flushKeyEvents deck = do
    report <- readKeyReport deck 0
    case report of
        Nothing -> pure ()
        Just _ -> flushKeyEvents deck

readKeyReport :: Deck -> Int -> IO (Maybe [KeyEvent])
readKeyReport (Deck device state _) timeout
    | timeout < 0 || toInteger timeout > 2147483647 =
        ioError (userError "Read timeout must be within 0..2147483647 milliseconds")
    | otherwise = modifyMVar state $ \previous -> do
        report <- Transport.readTimeout device 19 timeout
        if BS.null report then pure (previous, Nothing) else do
            current <- either (ioError . userError) pure (decodeKeyReport report)
            pure (current, Just (keyEvents previous current))

-- | Send a prepared image. Serialize complete images so their pages cannot interleave.
setKeyImage :: Deck -> Key -> KeyImage -> IO ()
setKeyImage (Deck device _ outputLock) key image = do
    reports <- either (ioError . userError) pure (imageReports key (imageBytes image))
    withMVar outputLock $ \_ -> forM_ reports $ \report -> do
        written <- Transport.write device report
        unless (written == BS.length report) $
            ioError (userError "Incomplete MK2 image report write")
