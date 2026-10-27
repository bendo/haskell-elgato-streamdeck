-- | Page definitions and a pure, session-persistent button state machine.
module StreamDeck.Pages
    ( Page (..), PageButton (..), ButtonState (..), Appearance (..)
    , button, statefulButton, state, refreshEvery
    , PageConfig, compilePages, DeckState, initialDeckState, currentPage, selectedState
    , visibleButtons, pressKey
    ) where

import Control.Monad (forM_, unless)
import Data.Char (isSpace)
import Data.List (find, nub)
import qualified Data.Map.Strict as Map
import StreamDeck.Action
import StreamDeck.Image (KeyImage)
import StreamDeck.Protocol (Key)

data Appearance = Static KeyImage | RefreshEvery Double (IO KeyImage)

data ButtonState = ButtonState
    { stateId :: StateId
    , stateAppearance :: Appearance
    , stateAction :: Action
    }

data PageButton = PageButton
    { pageButtonId :: ButtonId
    , pageButtonKey :: Key
    , initialState :: StateId
    , buttonStates :: [ButtonState]
    }

data Page = Page
    { pageId :: PageId
    , pageButtons :: [PageButton]
    }

-- | A one-state button with a prepared image (including font-generated images).
button :: ButtonId -> Key -> KeyImage -> Action -> PageButton
button name key image action = statefulButton name key "default" [state "default" (Static image) action]

statefulButton :: ButtonId -> Key -> StateId -> [ButtonState] -> PageButton
statefulButton = PageButton

state :: StateId -> Appearance -> Action -> ButtonState
state = ButtonState

-- | Refresh immediately on activation and then after each interval in seconds.
-- Only the currently visible state runs. Intervals must be finite and positive.
refreshEvery :: Double -> IO KeyImage -> Appearance
refreshEvery = RefreshEvery

data PageConfig = PageConfig PageId (Map.Map PageId Page)

data DeckState = DeckState
    { currentPage :: PageId
    , selections :: Map.Map (PageId, ButtonId) StateId
    } deriving (Eq, Show)

-- | Validate the complete graph, including actions in currently inactive states.
compilePages :: PageId -> [Page] -> Either String PageConfig
compilePages first pages = do
    unique "page ID" (map pageId pages)
    forM_ pages $ \page -> do
        validId "page" (pageId page)
        let buttons = pageButtons page
        unique ("button ID on page " ++ pageId page) (map pageButtonId buttons)
        unique ("key on page " ++ pageId page) (map pageButtonKey buttons)
        forM_ buttons $ \entry -> do
            validId "button" (pageButtonId entry)
            let states = buttonStates entry
                names = map stateId states
            unless (not (null states)) (Left ("Button has no states: " ++ pageButtonId entry))
            unique ("state ID in button " ++ pageButtonId entry) names
            unless (initialState entry `elem` names) (Left ("Unknown initial state in button " ++ pageButtonId entry))
            forM_ states $ \view -> do
                validId "state" (stateId view)
                case stateAppearance view of
                    Static _ -> pure ()
                    RefreshEvery seconds _ -> unless
                        (not (isNaN seconds || isInfinite seconds) && seconds > 0 &&
                         toRational seconds * 1000000 <= toRational (maxBound :: Int))
                        (Left "Refresh interval must be finite, positive, and fit in a threadDelay")
                validateAction names (stateAction view)
    unless (Map.member first table) (Left ("Unknown initial page: " ++ first))
    pure (PageConfig first table)
  where
    table = Map.fromList [(pageId page, page) | page <- pages]
    validateAction names action = case action of
        SwitchPage destination -> unless (Map.member destination table) (Left ("Unknown page: " ++ destination))
        NextState -> pure ()
        SetState destination -> unless (destination `elem` names) (Left ("Unknown button state: " ++ destination))
        Sequence actions -> mapM_ (validateAction names) actions
        effect -> validateEffect effect

unique :: (Eq a, Show a) => String -> [a] -> Either String ()
unique kind values = unless (length values == length (nub values)) (Left ("Duplicate " ++ kind ++ ": " ++ show values))

validId :: String -> String -> Either String ()
validId kind value = unless (not (null value) && not (all isSpace value)) (Left ("Empty " ++ kind ++ " ID"))

initialDeckState :: PageConfig -> DeckState
initialDeckState (PageConfig first pages) = DeckState first $ Map.fromList
    [ ((pageId page, pageButtonId entry), initialState entry)
    | page <- Map.elems pages, entry <- pageButtons page ]

selectedState :: DeckState -> PageId -> ButtonId -> Maybe StateId
selectedState model page name = Map.lookup (page, name) (selections model)

visibleButtons :: PageConfig -> DeckState -> [(PageButton, ButtonState)]
visibleButtons (PageConfig _ pages) model =
    [ (entry, view)
    | page <- maybe [] pure (Map.lookup (currentPage model) pages)
    , entry <- pageButtons page
    , view <- buttonStates entry
    , Just (stateId view) == selectedState model (pageId page) (pageButtonId entry)
    ]

-- | Resolve a press against the active page/state and return external effects.
-- State actions in a Sequence always target its originating button, even if the
-- same sequence switches pages. Effects are returned in their configured order.
pressKey :: PageConfig -> Key -> DeckState -> (DeckState, [Action])
pressKey config key model = case find ((== key) . pageButtonKey . fst) (visibleButtons config model) of
    Nothing -> (model, [])
    Just (entry, view) -> apply entry (currentPage model, pageButtonId entry) (model, []) (stateAction view)
  where
    apply entry reference (current, effects) action = case action of
        SwitchPage destination -> (current { currentPage = destination }, effects)
        SetState destination -> (current { selections = Map.insert reference destination (selections current) }, effects)
        NextState ->
            let names = map stateId (buttonStates entry)
                chosen = Map.lookup reference (selections current)
                following = case dropWhile (\name -> Just name /= chosen) names of
                    _ : next : _ -> Just next
                    _ -> case names of first : _ -> Just first; [] -> Nothing
            in case following of
                Nothing -> (current, effects)
                Just next -> (current { selections = Map.insert reference next (selections current) }, effects)
        Sequence actions -> foldl' (apply entry reference) (current, effects) actions
        effect -> (current, effects ++ [effect])
