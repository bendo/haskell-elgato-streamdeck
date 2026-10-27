-- | MK2 images: resize to 72x72, rotate for the device, then encode as JPEG.
module StreamDeck.Image
    ( KeyImage, imageBytes, prepareKeyImage, loadKeyImage, solidColor, saveKeyImagePreview
    , prepareImageReports
    ) where

import Codec.Picture
    ( Image, PixelRGB8 (..), PixelYCbCr8, convertRGB8, encodeJpegAtQuality
    , decodeJpeg, generateImage, imageWidth, imageHeight, pixelAt, readImage, writePng
    )
import Codec.Picture.Types (convertImage)
import Control.Exception (evaluate)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Lazy as Map
import Data.Word (Word8)
import StreamDeck.Protocol (Key, imageReports, keyAt)

-- Each key's packet variant is a lazy, shared value. Static images keep these
-- across page visits; dynamic render caches keep only their latest KeyImage.
data KeyImage = KeyImage BS.ByteString (Map.Map Key (Either String [BS.ByteString]))

-- | The native JPEG bytes, already oriented for the device.
imageBytes :: KeyImage -> BS.ByteString
imageBytes (KeyImage bytes _) = bytes

keyImage :: BS.ByteString -> KeyImage
keyImage bytes = KeyImage bytes $ Map.fromList
    [(key, imageReports key bytes) | row <- [0..2], column <- [0..4], Right key <- [keyAt row column]]

-- | Materialize a key's cached packet stream before beginning device I/O.
prepareImageReports :: Key -> KeyImage -> IO [BS.ByteString]
prepareImageReports key (KeyImage _ variants) = do
    reports <- either (ioError . userError) pure $
        Map.findWithDefault (Left "Invalid MK2 key") key variants
    mapM_ (evaluate . BS.length) reports
    pure reports

-- | Stretch an RGB image to a key using nearest-neighbour sampling.
-- Empty images are rejected. The caller supplies an upright image.
prepareKeyImage :: Image PixelRGB8 -> Either String KeyImage
prepareKeyImage source
    | width <= 0 || height <= 0 = Left "Key image dimensions must be positive"
    | otherwise = Right $ keyImage $ BL.toStrict $
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
solidColor red green blue = keyImage $ BL.toStrict $
    encodeJpegAtQuality 95 (convertImage rgb :: Image PixelYCbCr8)
  where
    rgb = generateImage (\_ _ -> PixelRGB8 red green blue) 72 72

-- | Save an upright PNG preview of the exact JPEG sent to the device.
saveKeyImagePreview :: FilePath -> KeyImage -> IO ()
saveKeyImagePreview path image = do
    native <- convertRGB8 <$> either (ioError . userError) pure (decodeJpeg (imageBytes image))
    writePng path $ generateImage (\x y -> pixelAt native (71 - x) (71 - y)) 72 72
