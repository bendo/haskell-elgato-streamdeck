module Main where

import           DeviceManager (enumerate, open, serialNumber)

main :: IO ()
main = do
    decks <- enumerate
    if null decks
        then print "No Stremdeck Found."
    else do
        print ("Found " ++ show (length decks) ++ " streamdeck(s).")
        let deckInfo = head decks
        print ("Serial number: " ++ serialNumber deckInfo)
        deck <- open deckInfo
        print "OK"
