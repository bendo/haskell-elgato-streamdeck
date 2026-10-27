module Main where

import           Control.Exception (catch)
import           Data.Maybe (fromMaybe)
import           DeviceManager (enumerate, serialNumber, withDevice, withHIDAPI)
import           System.Exit (die)
import           System.HIDAPI (HIDAPIException)

main :: IO ()
main = discover `catch` reportHIDError

reportHIDError :: HIDAPIException -> IO ()
reportHIDError err = die ("Stream Deck: " ++ show err)

discover :: IO ()
discover = withHIDAPI $ do
    decks <- enumerate
    case decks of
        [] -> putStrLn "No Stream Deck MK2 found."
        deckInfo : _ -> do
            putStrLn ("Found " ++ show (length decks) ++ " Stream Deck MK2 device(s).")
            putStrLn ("Serial number: " ++ fromMaybe "unknown" (serialNumber deckInfo))
            withDevice deckInfo $ \_ -> putStrLn "Device opened successfully."
