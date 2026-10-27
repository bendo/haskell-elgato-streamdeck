module Main where

import           Control.Exception (Handler (..), IOException, catches)
import           Control.Monad (forever)
import           Data.Maybe (fromMaybe)
import           DeviceManager (Deck, enumerate, readKeyEvents, serialNumber, setKeyImage, withDevice, withHIDAPI)
import           StreamDeck (keyAt, loadKeyImage)
import           System.Environment (getArgs)
import           System.Exit (die)
import           System.HIDAPI (HIDAPIException)
import           System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import           Text.Read (readMaybe)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    dispatch `catches` [Handler reportHIDError, Handler reportIOError]

reportHIDError :: HIDAPIException -> IO ()
reportHIDError err = die ("Stream Deck: " ++ show err)

reportIOError :: IOException -> IO ()
reportIOError err = die ("Stream Deck: " ++ show err)

dispatch :: IO ()
dispatch = do
    args <- getArgs
    case args of
        [] -> discover
        ["watch"] -> withFirstDeck $ \deck -> do
            putStrLn "Watching MK2 key events (zero-based indexes). Ctrl-C exits."
            forever $ readKeyEvents deck 100 >>= mapM_ print
        ["image", rowText, columnText, path] -> do
            row <- maybe (die "Row must be an integer") pure (readMaybe rowText)
            column <- maybe (die "Column must be an integer") pure (readMaybe columnText)
            key <- either die pure (keyAt row column)
            image <- loadKeyImage path
            withFirstDeck $ \deck -> setKeyImage deck key image
            putStrLn "Key image sent."
        _ -> die "Usage: haskell-elgato-streamdeck-exe [watch | image ROW COLUMN FILE]"

withFirstDeck :: (Deck -> IO ()) -> IO ()
withFirstDeck action = withHIDAPI $ do
    decks <- enumerate
    case decks of
        [] -> die "No Stream Deck MK2 found."
        deckInfo : _ -> withDevice deckInfo action

discover :: IO ()
discover = withHIDAPI $ do
    decks <- enumerate
    case decks of
        [] -> putStrLn "No Stream Deck MK2 found."
        deckInfo : _ -> do
            putStrLn ("Found " ++ show (length decks) ++ " Stream Deck MK2 device(s).")
            putStrLn ("Serial number: " ++ fromMaybe "unknown" (serialNumber deckInfo))
            withDevice deckInfo $ \_ -> putStrLn "Device opened successfully."
