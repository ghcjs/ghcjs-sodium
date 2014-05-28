{-# LANGUAGE OverloadedStrings #-}
-- | Events.
module Alder.Html.Events
    ( -- * Mouse events
      Button(..)
    , Modifier(..)
    , Position(..)
    , MouseEvent(..)
    ) where

import           Control.Applicative
import           Data.Aeson
import           Data.Maybe
import           Data.Set            as Set

-- | A mouse button.
data Button = LeftButton | MiddleButton | RightButton
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

-- | A mouse modifier key.
data Modifier = Alt | Ctrl | Meta | Shift
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

-- | Coordinate positions.
data Position = Position !Int !Int
    deriving (Eq, Ord, Read, Show)

-- | A mouse event.
data MouseEvent = MouseEvent
    { button         :: !Button
    , modifiers      :: !(Set Modifier)
    , clientPosition :: !Position
    , pagePosition   :: !Position
    , screenPosition :: !Position
    }

instance FromJSON MouseEvent where
    parseJSON = withObject "mouse event" $ \ev -> do
        enum <- ev .: "button"
        evButton <- case enum :: Int of
            0 -> return LeftButton
            1 -> return MiddleButton
            2 -> return RightButton
            _ -> fail "unknown mouse button"
        let switch a p = (\b -> if b then Just a else Nothing) <$> ev .: p
        evModifiers <- Set.fromList . catMaybes <$> sequence
            [ switch Alt   "altKey"
            , switch Ctrl  "ctrlKey"
            , switch Meta  "metaKey"
            , switch Shift "shiftKey"
            ]
        let position a b = Position <$> ev .: a <*> ev .: b
        clientPos <- position "clientX" "clientY"
        pagePos   <- position "pageX" "pageY"
        screenPos <- position "screenX" "screenY"
        return MouseEvent
            { button         = evButton
            , modifiers      = evModifiers
            , clientPosition = clientPos
            , pagePosition   = pagePos
            , screenPosition = screenPos
            }
