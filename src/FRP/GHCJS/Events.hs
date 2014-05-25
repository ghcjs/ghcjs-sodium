{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
-- | Events.
module FRP.GHCJS.Events
    ( -- * Events
      Events
    , HasEvents(..)
      -- * Mouse events
    , MouseEvent
    , HasMouseEvent(..)
    , Button(..)
    , Modifier(..)
    , Position(..)
      -- * Input events
    , InputEvent
    , HasInputEvent(..)
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Maybe
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           GHC.Generics
import qualified GHCJS.DOM.Element          as DOM
import qualified GHCJS.DOM.Event            as DOM
import qualified GHCJS.DOM.HTMLInputElement as DOM
import qualified GHCJS.DOM.MouseEvent       as DOM

import           Data.Default
import           FRP.GHCJS.Input
import           FRP.GHCJS.Internal.Events

-- | A mouse button.
data Button = LeftButton | MiddleButton | RightButton
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

-- | A mouse modifier key.
data Modifier = Alt | Ctrl | Meta | Shift
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

-- | Coordinate positions.
data Position = Position !Int !Int
    deriving (Eq, Ord, Read, Show, Generic)

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
    extractEvent ev = do
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

-- | An input event.
data InputEvent = InputEvent
    { _checked :: !Bool
    , _value   :: !Text
    }

makeClassy ''InputEvent

instance Event InputEvent where
    extractEvent ev = do
        Just target <- DOM.eventGetTarget ev
        let e = DOM.castToHTMLInputElement target
        oldChecked <- not . Text.null <$>
            DOM.elementGetAttribute e ("checked" :: Text)
        oldValue   <- DOM.elementGetAttribute e ("value" :: Text)
        newChecked <- DOM.htmlInputElementGetChecked e
        newValue   <- DOM.htmlInputElementGetValue e
        -- revert values to original state
        when (oldChecked /= newChecked) $
            DOM.htmlInputElementSetChecked e newChecked
        when (oldValue /= newValue) $
            DOM.htmlInputElementSetValue e newValue
        return InputEvent
            { _checked = newChecked
            , _value   = newValue
            }

-- | A set of event handlers for an element.
data Events = Events
    { _change      :: Input InputEvent
    , _click       :: Input MouseEvent
    , _doubleClick :: Input MouseEvent
    , _drag        :: Input MouseEvent
    , _dragEnd     :: Input MouseEvent
    , _dragEnter   :: Input MouseEvent
    , _dragExit    :: Input MouseEvent
    , _dragLeave   :: Input MouseEvent
    , _dragOver    :: Input MouseEvent
    , _dragStart   :: Input MouseEvent
    , _drop        :: Input MouseEvent
    , _mouseDown   :: Input MouseEvent
    , _mouseEnter  :: Input MouseEvent
    , _mouseLeave  :: Input MouseEvent
    , _mouseMove   :: Input MouseEvent
    , _mouseOut    :: Input MouseEvent
    , _mouseOver   :: Input MouseEvent
    , _mouseUp     :: Input MouseEvent
    } deriving (Generic)

instance Default Events

makeClassy ''Events
