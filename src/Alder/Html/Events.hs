{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
-- | Events.
module Alder.Html.Events
    ( Event(..)
      -- * Mouse events
    , Button(..)
    , Modifier(..)
    , Position(..)
    , MouseEvent
    , HasMouseEvent(..)
    ) where

import           Control.Applicative
import           Control.Lens
import           Data.Maybe
import           Data.Set            (Set)
import qualified Data.Set            as Set

import           Alder.JavaScript
import           Alder.Types

class Event a where
    extractEvent :: Text -> DOMEvent -> IO a

-- | A mouse button.
data Button = LeftButton | MiddleButton | RightButton
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

-- | A mouse modifier key.
data Modifier = Alt | Ctrl | Meta | Shift
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

-- | Coordinate positions.
data Position = Position !Int !Int
    deriving (Eq, Ord, Read, Show)

instance Field1 Position Position Int Int where
    _1 k (Position x y) = indexed k (0 :: Int) x <&> \x' -> Position x' y

instance Field2 Position Position Int Int where
    _2 k (Position x y) = indexed k (1 :: Int) y <&> \y' -> Position x y'

-- | A mouse event.
data MouseEvent = MouseEvent
    { _button         :: !Button
    , _modifiers      :: !(Set Modifier)
    , _clientPosition :: !Position
    , _pagePosition   :: !Position
    , _screenPosition :: !Position
    }

makeClassy ''MouseEvent

instance Event MouseEvent where
    extractEvent _ ev = do
        evButton <- ev ! "button" <&> \i -> case i :: Int of
            0 -> LeftButton
            1 -> MiddleButton
            _ -> RightButton
        let switch a p = (\b -> if b then Just a else Nothing) <$> ev ! p
        evModifiers <- Set.fromList . catMaybes <$> sequence
            [ switch Alt   "altKey"
            , switch Ctrl  "ctrlKey"
            , switch Meta  "metaKey"
            , switch Shift "shiftKey"
            ]
        let position a b = Position <$> ev ! a <*> ev ! b
        clientPos <- position "clientX" "clientY"
        pagePos   <- position "x" "y"
        screenPos <- position "screenX" "screenY"
        return MouseEvent
            { _button         = evButton
            , _modifiers      = evModifiers
            , _clientPosition = clientPos
            , _pagePosition   = pagePos
            , _screenPosition = screenPos
            }
