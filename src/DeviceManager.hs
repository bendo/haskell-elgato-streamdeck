module DeviceManager
  ( enumerate
  , open
  , serialNumber
  )
where

import qualified Data.ByteString as BS
import qualified Data.Word       as DW
import qualified System.HIDAPI   as HID

-- StreamDeckMini 0x0063
-- StreamDeckOriginal 0x0060
-- StreamDeckMK2 0x0080
-- StreamDeckXL 0x006c

data DeckType = StreamDeckMini | StreamDeck | StreamDeckMK2 | StreamDeckXL

data Deck = Deck { ref  :: HID.Device
                 , info :: HID.DeviceInfo
                 }

vendorId :: DW.Word16
vendorId = 0x0fd9

productId :: DeckType -> HID.ProductID
productId StreamDeckMini = 0x0063
productId StreamDeck     = 0x0060
productId StreamDeckMK2  = 0x0080
productId StreamDeckXL   = 0x006c

enumerate :: IO [HID.DeviceInfo]
enumerate = HID.enumerate (Just vendorId) (Just $ productId StreamDeckMK2)

open :: HID.DeviceInfo -> IO Deck
open deviceInfo = HID.withHIDAPI $ do
        deck <- HID.openDeviceInfo deviceInfo
        return Deck { ref = deck, info = deviceInfo }

serialNumber :: HID.DeviceInfo -> Maybe String
serialNumber = HID.serialNumber

firmwareVersion :: Deck -> IO (DW.Word8, BS.ByteString)
firmwareVersion deck = HID.getFeatureReport (ref deck) 0x06 32

reset :: Deck -> IO()
reset deck = undefined
