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
  )
where

import           Control.Exception (bracket)
import qualified Data.ByteString as BS
import qualified Data.Word       as DW
import           System.HIDAPI   (DeviceInfo)
import qualified System.HIDAPI   as HID

-- StreamDeckMini 0x0063
-- StreamDeckOriginal 0x0060
-- StreamDeckMK2 0x0080
-- StreamDeckXL 0x006c

data DeckType = StreamDeckMini | StreamDeck | StreamDeckMK2 | StreamDeckXL

newtype Deck = Deck HID.Device

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
open deviceInfo = Deck <$> HID.openDeviceInfo deviceInfo

-- | Close an open device exactly once, before leaving 'withHIDAPI'.
close :: Deck -> IO ()
close (Deck device) = HID.close device

serialNumber :: DeviceInfo -> Maybe String
serialNumber = HID.serialNumber

-- | Read the raw MK2 firmware feature report (report ID and payload).
firmwareVersion :: Deck -> IO (DW.Word8, BS.ByteString)
firmwareVersion (Deck device) = HID.getFeatureReport device 0x06 32
