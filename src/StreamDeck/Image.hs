-- | MK2 images: resize to 72x72, rotate for the device, then encode as JPEG.
module StreamDeck.Image
    ( KeyImage, imageBytes, prepareKeyImage, loadKeyImage, solidColor
    ) where

import Codec.Picture
    ( Image, PixelRGB8 (..), PixelYCbCr8, convertRGB8, encodeJpegAtQuality
    , generateImage, imageWidth, imageHeight, pixelAt, readImage
    )
import Codec.Picture.Types (convertImage)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)

newtype KeyImage = KeyImage BS.ByteString

-- | The native JPEG bytes, already oriented for the device.
imageBytes :: KeyImage -> BS.ByteString
imageBytes (KeyImage bytes) = bytes

-- | Stretch an RGB image to a key using nearest-neighbour sampling.
-- Empty images are rejected. The caller supplies an upright image.
prepareKeyImage :: Image PixelRGB8 -> Either String KeyImage
prepareKeyImage source
    | width <= 0 || height <= 0 = Left "Key image dimensions must be positive"
    | otherwise = Right $ KeyImage $ BL.toStrict $
        encodeJpegAtQuality 95 (convertImage native :: Image PixelYCbCr8)
  where
    width = imageWidth source
    height = imageHeight source
    native = generateImage sample 72 72
    sample x y = pixelAt source ((71 - x) * width `div` 72)
                                ((71 - y) * height `div` 72)

-- | Load an image supported by JuicyPixels, including PNG and JPEG.
loadKeyImage :: FilePath -> IO KeyImage
loadKeyImage path = do
    decoded <- readImage path
    either (ioError . userError . ((path ++ ": ") ++)) pure $
        decoded >>= prepareKeyImage . convertRGB8

solidColor :: Word8 -> Word8 -> Word8 -> KeyImage
solidColor red green blue = KeyImage $ BL.toStrict $
    encodeJpegAtQuality 95 (convertImage rgb :: Image PixelYCbCr8)
  where
    rgb = generateImage (\_ _ -> PixelRGB8 red green blue) 72 72
