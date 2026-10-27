module Main where

import Control.Monad (forM, forM_)
import StreamDeck
import qualified StreamDeck.FontAwesome as FA
import System.Environment (getArgs)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    fonts <- FA.loadFontAwesome
    buttons <- forM (zip [0..] icons) $ \(index, (name, icon, color)) -> do
        key <- either fail pure (keyAt (index `div` 5) (index `mod` 5))
        image <- either fail pure $ FA.renderAwesomeIcon fonts
            defaultIconStyle { foreground = color } icon
        pure (name, Button key image (putStrLn name))
    args <- getArgs
    case args of
        [] -> do
            putStrLn "Font Awesome: Spotify, volume, and weather icons on the first two rows."
            putStrLn "Press a button to print its name. Ctrl-C exits."
            runDeck (map snd buttons)
        ["--preview", prefix] -> forM_ buttons $ \(name, button) -> do
            let path = prefix ++ "-" ++ name ++ ".png"
            saveKeyImagePreview path (buttonImage button)
            putStrLn path
        _ -> fail "Usage: FontAwesome.hs [--preview OUTPUT_PREFIX]"

icons :: [(String, FA.AwesomeIcon, Color)]
icons =
    [ ("spotify", FA.spotify, RGB 30 215 96)
    , ("volume", FA.volumeHigh, RGB 255 255 255)
    , ("sun", FA.sun, RGB 255 210 40)
    , ("rain", FA.cloudRain, RGB 60 170 255)
    , ("snow", FA.snowflake, RGB 140 230 255)
    , ("cloud-sun", FA.cloudSun, RGB 255 190 80)
    , ("wind", FA.wind, RGB 180 220 240)
    , ("lightning", FA.bolt, RGB 255 230 60)
    , ("temperature", FA.temperatureHalf, RGB 255 100 70)
    , ("radioactive", FA.radiation, RGB 180 255 40)
    ]
