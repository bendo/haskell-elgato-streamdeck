module FontSpec (tests) where

import Codec.Picture
    ( Image, PixelRGB8 (..), convertRGB8, decodeJpeg, generateImage
    , imageWidth, imageHeight, pixelAt, readImage )
import Control.Exception (bracket)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import StreamDeck.Font
import qualified StreamDeck.FontAwesome as FA
import StreamDeck.Image (KeyImage, imageBytes, prepareKeyImage, saveKeyImagePreview)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openBinaryTempFile)
import Test.HUnit (Test (..), (~:), assertBool, assertEqual, assertFailure)

tests :: Test
tests = TestList
    [ "bundled fonts and named glyphs" ~: TestCase $ do
        fonts <- FA.loadFontAwesome
        forM_ namedIcons $ \icon -> do
            image <- right (FA.renderAwesomeIcon fonts defaultIconStyle icon)
            decoded <- decode image
            assertEqual "JPEG dimensions" (72, 72) (imageWidth decoded, imageHeight decoded)
            _ <- inkBounds decoded
            pure ()
        regular <- right $ FA.renderAwesomeIcon fonts defaultIconStyle (FA.AwesomeIcon FA.Regular '\xf185')
        _ <- inkBounds =<< decode regular
        assertBool "Spotify belongs to brands"
            (hasGlyph (FA.brandsFont fonts) '\xf1bc')
        assertBool "wrong family fails clearly" $ isLeft $
            FA.renderAwesomeIcon fonts defaultIconStyle (FA.AwesomeIcon FA.Solid '\xf1bc')
    , "glyph fitting and centering" ~: TestCase $ do
        fonts <- FA.loadFontAwesome
        forM_ namedIcons $ \icon -> do
            image <- right $ FA.renderAwesomeIcon fonts defaultIconStyle { padding = 12 } icon
            (minX, minY, maxX, maxY) <- inkBounds =<< decode image
            assertBool ("left/right padding: " ++ show icon) (minX >= 11 && maxX <= 60)
            assertBool ("top/bottom padding: " ++ show icon) (minY >= 11 && maxY <= 60)
            assertBool ("horizontal centering: " ++ show icon) (abs (minX + maxX - 71) <= 2)
            assertBool ("vertical centering: " ++ show icon) (abs (minY + maxY - 71) <= 2)
            assertBool ("fills padded area: " ++ show icon)
                (max (maxX - minX + 1) (maxY - minY + 1) >= 44)
        wide <- right $ FA.renderAwesomeIcon fonts defaultIconStyle FA.volumeHigh
        (x0, y0, x1, y1) <- inkBounds =<< decode wide
        assertBool "preserve wide icon aspect ratio" (x1 - x0 > y1 - y0)
    , "font colors and padding" ~: TestCase $ do
        fonts <- FA.loadFontAwesome
        let style = defaultIconStyle { foreground = RGB 20 200 80, background = RGB 10 20 40 }
        colored <- decode =<< right (FA.renderAwesomeIcon fonts style FA.spotify)
        let PixelRGB8 red green blue = pixelAt colored 0 0
        assertBool "background color survives JPEG encoding"
            (abs (fromIntegral red - 10 :: Int) < 12 &&
             abs (fromIntegral green - 20 :: Int) < 12 &&
             abs (fromIntegral blue - 40 :: Int) < 12)
        assertBool "foreground color survives JPEG encoding" $ or
            [ green' > 160 && red' < 60 && blue' < 120
            | y <- [0..71], x <- [0..71], let PixelRGB8 red' green' blue' = pixelAt colored x y ]
        large <- right $ FA.renderAwesomeIcon fonts defaultIconStyle { padding = 4 } FA.sun
        small <- right $ FA.renderAwesomeIcon fonts defaultIconStyle { padding = 20 } FA.sun
        (lx0, _, lx1, _) <- inkBounds =<< decode large
        (sx0, _, sx1, _) <- inkBounds =<< decode small
        assertBool "larger padding shrinks icon" (lx1 - lx0 > sx1 - sx0 + 20)
    , "single-line labels" ~: TestCase $ do
        fonts <- FA.loadFontAwesome
        forM_ ["PLAY", "RADIO", "12"] $ \text -> do
            image <- right $ renderText (FA.solidFont fonts) defaultIconStyle text
            (minX, minY, maxX, maxY) <- inkBounds =<< decode image
            assertBool "label stays inside padding" (minX >= 7 && minY >= 7 && maxX <= 64 && maxY <= 64)
            assertBool "label is centered" (abs (minX + maxX - 71) <= 2 && abs (minY + maxY - 71) <= 2)
    , "font input validation" ~: TestCase $ do
        fonts <- FA.loadFontAwesome
        let font = FA.solidFont fonts
        forM_ [-1, 36, maxBound] $ \invalid -> assertBool "reject invalid padding" $
            isLeft (renderGlyph font defaultIconStyle { padding = invalid } '\xf185')
        forM_ ["", " ", "PLAY\n", "\t", "\x10ffff"] $ \invalid ->
            assertBool "reject blank, multiline, control, or missing glyph" $
                isLeft (renderText font defaultIconStyle invalid)
        assertBool "reject invalid font data" (isLeft (decodeFont BS.empty))
    , "PNG preview is upright" ~: TestCase $ withPreviewFile $ \path -> do
        image <- right $ prepareKeyImage $ generateImage
            (\x y -> if x < 36 && y < 36 then PixelRGB8 240 10 10 else PixelRGB8 10 20 240) 72 72
        saveKeyImagePreview path image
        bytes <- BS.readFile path
        assertEqual "PNG signature" (BS.pack [137, 80, 78, 71, 13, 10, 26, 10]) (BS.take 8 bytes)
        preview <- convertRGB8 <$> (readImage path >>= right)
        native <- decode image
        assertEqual "preview dimensions" (72, 72) (imageWidth preview, imageHeight preview)
        forM_ [(5, 5), (66, 66), (5, 66), (66, 5)] $ \(x, y) ->
            assertEqual "undo device rotation exactly" (pixelAt native (71 - x) (71 - y)) (pixelAt preview x y)
    ]

namedIcons :: [FA.AwesomeIcon]
namedIcons =
    [ FA.spotify, FA.volumeHigh, FA.volumeLow, FA.volumeXmark
    , FA.sun, FA.cloud, FA.cloudSun, FA.cloudRain, FA.cloudShowersHeavy
    , FA.snowflake, FA.wind, FA.bolt, FA.temperatureHalf, FA.radiation
    , FA.play, FA.pause, FA.forwardStep, FA.backwardStep, FA.headphones, FA.terminal, FA.heart
    ]

decode :: KeyImage -> IO (Image PixelRGB8)
decode image = convertRGB8 <$> right (decodeJpeg (imageBytes image))

inkBounds :: Image PixelRGB8 -> IO (Int, Int, Int, Int)
inkBounds image = case
    [ (x, y) | y <- [0..71], x <- [0..71]
    , let PixelRGB8 red green blue = pixelAt image x y
    , max red (max green blue) > 100 ] of
        [] -> assertFailure "Expected visible glyph pixels" >> fail "Empty glyph"
        (x, y) : rest -> pure $ foldl' step (x, y, x, y) rest
  where
    step (minX, minY, maxX, maxY) (x, y) =
        (min minX x, min minY y, max maxX x, max maxY y)

right :: Either String a -> IO a
right = either (ioError . userError) pure

withPreviewFile :: (FilePath -> IO a) -> IO a
withPreviewFile = bracket create removeFile
  where
    create = do
        directory <- getTemporaryDirectory
        (path, handle) <- openBinaryTempFile directory "streamdeck-preview.png"
        hClose handle
        pure path
