-- | A small single-file application API for the Stream Deck MK2.
module StreamDeck
    ( Button (..), runDeck
    , runPages
    , Page (..), PageButton (..), ButtonState (..), Appearance (..)
    , button, statefulButton, state, refreshEvery
    , Action (..), PageId, ButtonId, StateId, KeyChord (..), Modifier (..), super
    , ClockZone, loadClockZone, loadLocalClockZone, formatClock, clockImage
    , Key, keyAt, keyIndex, keyPosition, KeyEvent (..)
    , KeyImage, loadKeyImage, solidColor
    , saveKeyImagePreview
    , Font, loadFont, Color (..), IconStyle (..), defaultIconStyle, renderGlyph, renderText
    ) where

import Control.Monad (forM_, unless)
import Data.List (nub)
import qualified DeviceManager as Device
import StreamDeck.Action
import StreamDeck.Clock
import StreamDeck.Font (Font, loadFont, Color (..), IconStyle (..), defaultIconStyle, renderGlyph, renderText)
import StreamDeck.Image (KeyImage, loadKeyImage, solidColor, saveKeyImagePreview)
import StreamDeck.Protocol (Key, KeyEvent (..), keyAt, keyIndex, keyPosition)
import StreamDeck.Pages
import StreamDeck.Pages.Runtime (PageDriver (..), runPageRuntime)
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
                forM_ buttons $ \entry ->
                    Device.setKeyImage deck (buttonKey entry) (buttonImage entry)
                runEventLoop (Device.readKeyEvents deck 100)
                    [(buttonKey entry, onPress entry) | entry <- buttons]

-- | Run validated pages, remembering each button's selected state for this session.
-- All 15 keys belong to the page layout; unconfigured keys are drawn black.
runPages :: PageId -> [Page] -> IO ()
runPages first pages = do
    config <- either (ioError . userError) pure (compilePages first pages)
    Device.withHIDAPI $ do
        devices <- Device.enumerate
        case devices of
            [] -> ioError (userError "No Stream Deck MK2 found")
            device : _ -> Device.withDevice device $ \deck ->
                runPageRuntime
                    (PageDriver (Device.readKeyEvents deck 100) (Device.flushKeyEvents deck) (Device.setKeyImage deck))
                    (const (pure ())) config
