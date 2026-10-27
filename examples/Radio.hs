module Main where

import StreamDeck
import System.Environment (getEnv)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    home <- getEnv "HOME"
    image <- loadKeyImage (home ++ "/radio.jpeg")
    first <- either fail pure (keyAt 0 0)
    putStrLn "Press the top-left button to print radioactive. Ctrl-C exits."
    runDeck [Button first image (putStrLn "radioactive")]
