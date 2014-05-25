{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
-- | Events.
module FRP.GHCJS.Event
    ( -- * Mouse events
      Button(..)
    , Modifier(..)
    , Position(..)
    , MouseEvent
    , HasMouseEvent(..)
    ) where

import           Control.Applicative
import           Control.Lens
import           Data.Maybe
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import qualified GHCJS.DOM.MouseEvent     as DOM

import           FRP.GHCJS.Internal.Event

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
        let e = DOM.castToMouseEvent ev
        evButton <- DOM.mouseEventGetButton e <&> \i -> case i of
            2 -> RightButton
            1 -> MiddleButton
            _ -> LeftButton
        let switch a b = if b then Just a else Nothing
        evModifiers <- Set.fromList . catMaybes <$> sequence
            [ switch Alt   <$> DOM.mouseEventGetAltKey e
            , switch Ctrl  <$> DOM.mouseEventGetCtrlKey e
            , switch Meta  <$> DOM.mouseEventGetMetaKey e
            , switch Shift <$> DOM.mouseEventGetShiftKey e
            ]
        clientPos <- Position <$> DOM.mouseEventGetClientX e
                              <*> DOM.mouseEventGetClientY e
        pagePos   <- Position <$> DOM.mouseEventGetX e
                              <*> DOM.mouseEventGetY e
        screenPos <- Position <$> DOM.mouseEventGetScreenX e
                              <*> DOM.mouseEventGetScreenY e
        return MouseEvent
            { _button         = evButton
            , _modifiers      = evModifiers
            , _clientPosition = clientPos
            , _pagePosition   = pagePos
            , _screenPosition = screenPos
            }
