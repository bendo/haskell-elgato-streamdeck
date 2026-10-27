-- | Antialiased TrueType icons, labels, and two-line readouts fitted to a key.
module StreamDeck.Font
    ( Font, loadFont, loadTextFont, decodeFont, hasGlyph
    , Color (..), IconStyle (..), defaultIconStyle
    , renderGlyph, renderText, renderLabeledText
    ) where

import Codec.Picture (DynamicImage (ImageRGBA8), PixelRGBA8 (..), convertRGB8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isControl)
import qualified Data.Vector.Unboxed as V
import Data.Word (Word8)
import Graphics.Rasterific (V2 (..), printTextAt, renderDrawingAtDpi, withTexture, withTransformation)
import Graphics.Rasterific.Texture (uniformTexture)
import Graphics.Rasterific.Transformations (scale, translate)
import qualified Graphics.Text.TrueType as TrueType
import Paths_haskell_elgato_streamdeck (getDataFileName)
import StreamDeck.Image (KeyImage, prepareKeyImage)

newtype Font = Font TrueType.Font

data Color = RGB Word8 Word8 Word8 deriving (Eq, Show)

data IconStyle = IconStyle
    { foreground :: Color
    , background :: Color
    , padding :: Int
    } deriving (Eq, Show)

defaultIconStyle :: IconStyle
defaultIconStyle = IconStyle (RGB 255 255 255) (RGB 0 0 0) 8

-- | Load a .ttf file once and reuse the returned font for many buttons.
loadFont :: FilePath -> IO Font
loadFont path = do
    bytes <- BS.readFile path
    either (ioError . userError . ((path ++ ": ") ++)) pure (decodeFont bytes)

-- | Bundled DejaVu Sans, including lowercase letters, punctuation, and tabular digits.
loadTextFont :: IO Font
loadTextFont = getDataFileName "fonts/dejavu/DejaVuSans.ttf" >>= loadFont

decodeFont :: BS.ByteString -> Either String Font
decodeFont bytes = Font <$> TrueType.decodeFont (BL.fromStrict bytes)

hasGlyph :: Font -> Char -> Bool
hasGlyph (Font font) character = not (TrueType.isPlaceholder font character)

renderGlyph :: Font -> IconStyle -> Char -> Either String KeyImage
renderGlyph font style character = renderText font style [character]

-- | Fit the visible outline into the padded area, preserving aspect ratio.
-- Missing glyphs and empty/blank/multiline labels are rejected. This performs
-- simple font layout, not complex-script shaping or system font fallback.
renderText :: Font -> IconStyle -> String -> Either String KeyImage
renderText loaded@(Font font) style text
    | padding style < 0 || padding style > 35 = Left "Icon padding must be within 0..35 pixels"
    | null text = Left "Text must not be empty"
    | any isControl text = Left "Only single-line text without control characters is supported"
    | missing : _ <- filter (not . hasGlyph loaded) text =
        Left ("Font does not contain glyph " ++ show missing)
    | otherwise = case points of
        [] -> Left "Text has no drawable glyphs"
        first : rest -> do
            let (minX, minY, maxX, maxY) = foldl' bounds (fst first, snd first, fst first, snd first) rest
                width = maxX - minX
                height = maxY - minY
                available = fromIntegral (72 - 2 * padding style)
                factor = min (available / width) (available / height)
                offsetX = (72 - factor * width) / 2 - factor * minX
                offsetY = (72 - factor * height) / 2 - factor * minY
                placement = translate (V2 offsetX offsetY) <> scale factor factor
            if width <= 0 || height <= 0
                then Left "Text has no drawable area"
                else prepareKeyImage $ convertRGB8 $ ImageRGBA8 $
                    renderDrawingAtDpi 72 72 72 (rgba (background style)) $
                        withTexture (uniformTexture (rgba (foreground style))) $
                            withTransformation placement $
                                printTextAt font (TrueType.PointSize 72) (V2 0 0) text
  where
    points = concatMap V.toList $ concat $
        TrueType.getStringCurveAtPoint 72 (0, 0) [(font, TrueType.PointSize 72, text)]
    bounds (minX, minY, maxX, maxY) (x, y) =
        (min minX x, min minY y, max maxX x, max maxY y)

rgba :: Color -> PixelRGBA8
rgba (RGB red green blue) = PixelRGBA8 red green blue 255

-- | A small centered title above a larger value. Each line is fitted independently
-- without stretching. Logical advances and a shared digit baseline keep clock
-- values in the bundled font stable as their digits change.
renderLabeledText :: Font -> IconStyle -> String -> String -> Either String KeyImage
renderLabeledText loaded@(Font font) style title value = do
    if padding style < 0 || padding style > 35
        then Left "Icon padding must be within 0..35 pixels" else pure ()
    titleBounds <- checkedBounds title
    valueBounds <- checkedBounds value
    let digitReference = filter (hasGlyph loaded) "0123456789:"
        (_, referenceMinY, _, referenceMaxY) =
            either (const valueBounds) id (checkedBounds digitReference)
        (_, valueMinY, _, valueMaxY) = valueBounds
        stableValueBounds = (min referenceMinY valueMinY, max referenceMaxY valueMaxY)
        inset = fromIntegral (padding style)
        available = 72 - 2 * inset
        gap = min 4 (available / 8)
        split = inset + available * 0.4
        line maximumSize top bottom text (minX, _, maxX, _) (lowY, highY) =
            let advance = TrueType._xMax (TrueType.stringBoundingBox font 72 (TrueType.PointSize 72) text)
                width = max advance (maxX - minX)
                factor = minimum [maximumSize / 72, max 1 (available - 2) / width, (bottom - top) / (highY - lowY)]
                offsetX = (72 - factor * width) / 2 - factor * min 0 minX
                offsetY = (top + bottom - factor * (lowY + highY)) / 2
            in withTransformation (translate (V2 offsetX offsetY) <> scale factor factor) $
                printTextAt font (TrueType.PointSize 72) (V2 0 0) text
        (_, titleMinY, _, titleMaxY) = titleBounds
    prepareKeyImage $ convertRGB8 $ ImageRGBA8 $
        renderDrawingAtDpi 72 72 72 (rgba (background style)) $
            withTexture (uniformTexture (rgba (foreground style))) $ do
                line 14 inset (split - gap / 2) title titleBounds (titleMinY, titleMaxY)
                line 24 (split + gap / 2) (72 - inset) value valueBounds stableValueBounds
  where
    checkedBounds text
        | null text = Left "Text must not be empty"
        | any isControl text = Left "Each labeled-text line must contain no control characters"
        | missing : _ <- filter (not . hasGlyph loaded) text = Left ("Font does not contain glyph " ++ show missing)
        | otherwise = case concatMap V.toList $ concat $
                TrueType.getStringCurveAtPoint 72 (0, 0) [(font, TrueType.PointSize 72, text)] of
            [] -> Left "Text has no drawable glyphs"
            (x, y) : rest ->
                let result@(minX, minY, maxX, maxY) = foldl'
                        (\(left, top, right, bottom) (px, py) ->
                            (min left px, min top py, max right px, max bottom py)) (x, y, x, y) rest
                in if maxX <= minX || maxY <= minY then Left "Text has no drawable area" else Right result
