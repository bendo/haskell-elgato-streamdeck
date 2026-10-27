module Main where

import StreamDeck
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hello <- either fail pure (keyAt 0 0)
    orange <- either fail pure (keyAt 0 1)
    putStrLn "Configuring the first two keys. Press them to run actions; Ctrl-C exits."
    runDeck
        [ Button hello (solidColor 30 120 220) (putStrLn "Hello from your Stream Deck!")
        , Button orange (solidColor 220 100 30) (putStrLn "You pressed the orange key.")
        ]
