-- | A small single-file application API for the Stream Deck MK2.
module StreamDeck
    ( Button (..), runDeck
    , Key, keyAt, keyIndex, keyPosition, KeyEvent (..)
    , KeyImage, loadKeyImage, solidColor
    ) where

import Control.Monad (forM_, unless)
import Data.List (nub)
import qualified DeviceManager as Device
import StreamDeck.Image (KeyImage, loadKeyImage, solidColor)
import StreamDeck.Protocol (Key, KeyEvent (..), keyAt, keyIndex, keyPosition)
import StreamDeck.Runtime (runEventLoop)

data Button = Button
    { buttonKey :: Key
    , buttonImage :: KeyImage
    , onPress :: IO ()
    }

-- | Open the first MK2, render the configured keys, then dispatch press actions.
-- Actions are queued and run sequentially on a worker separate from input reading.
-- Synchronous action errors are logged; Ctrl-C cancels the worker and closes the device.
-- Unconfigured keys are left untouched. No reset is sent on exit.
runDeck :: [Button] -> IO ()
runDeck buttons = do
    let keys = map buttonKey buttons
    unless (length keys == length (nub keys)) $
        ioError (userError "A key can only have one button binding")
    Device.withHIDAPI $ do
        devices <- Device.enumerate
        case devices of
            [] -> ioError (userError "No Stream Deck MK2 found")
            device : _ -> Device.withDevice device $ \deck -> do
                forM_ buttons $ \button ->
                    Device.setKeyImage deck (buttonKey button) (buttonImage button)
                runEventLoop (Device.readKeyEvents deck 100)
                    [(buttonKey button, onPress button) | button <- buttons]
