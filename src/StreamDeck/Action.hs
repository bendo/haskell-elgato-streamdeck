-- | Declarative press actions. Page/state actions are interpreted by the page runtime.
module StreamDeck.Action
    ( Action (..), PageId, ButtonId, StateId
    , KeyChord (..), Modifier (..), super, keyChordText
    , validateEffect, performEffect
    ) where

import Control.Monad (unless)
import Data.Char (isAscii, isAlphaNum)
import Data.List (intercalate, nub)
import StreamDeck.Keyboard.X11 (sendKeyNames)
import System.Process (callCommand, callProcess)

type PageId = String
type ButtonId = String
type StateId = String

data Action
    = SwitchPage PageId
    | Execute FilePath [String]
    | Shell String
    | SendKeys KeyChord
    | RunIO (IO ())
    | NextState
    | SetState StateId
    | Sequence [Action]

data Modifier = Super | Control | Alt | Shift deriving (Eq, Ord, Show)
data KeyChord = KeyChord [Modifier] String deriving (Eq, Show)

super :: Char -> KeyChord
super character = KeyChord [Super] [character]

-- | An X11 keysym name, e.g. d, Return, F12, or XF86AudioRaiseVolume.
keyChordText :: KeyChord -> Either String String
keyChordText chord = intercalate "+" <$> keyChordNames chord

keyChordNames :: KeyChord -> Either String [String]
keyChordNames (KeyChord modifiers key)
    | null key || not (all valid key) = Left "Key must be an ASCII X11 keysym name (letters, digits, underscore)"
    | length modifiers /= length (nub modifiers) = Left "Duplicate keyboard modifier"
    | otherwise = Right (map modifier modifiers ++ [key])
  where
    valid character = isAscii character && (isAlphaNum character || character == '_')
    modifier Super = "Super_L"
    modifier Control = "Control_L"
    modifier Alt = "Alt_L"
    modifier Shift = "Shift_L"

validateEffect :: Action -> Either String ()
validateEffect (Execute command arguments) = do
    unless (not (null command) && '\0' `notElem` command) (Left "Command must be a nonempty executable name or path")
    unless (all (notElem '\0') arguments) (Left "Command arguments must not contain NUL")
validateEffect (Shell command) =
    unless (not (null command) && '\0' `notElem` command) (Left "Shell command must be nonempty and contain no NUL")
validateEffect (SendKeys chord) = () <$ keyChordText chord
validateEffect (RunIO _) = Right ()
validateEffect _ = Left "Expected an external effect action"

-- | Execute one external effect. Called on the runtime's worker, never its UI loop.
-- Execute passes arguments directly; Shell explicitly opts into shell syntax.
performEffect :: Action -> IO ()
performEffect action = do
    either (ioError . userError) pure (validateEffect action)
    case action of
        Execute command arguments -> callProcess command arguments
        Shell command -> callCommand command
        SendKeys chord -> do
            names <- either (ioError . userError) pure (keyChordNames chord)
            sendKeyNames names
        RunIO operation -> operation
        _ -> ioError (userError "Page/state actions must be interpreted by runPages")
