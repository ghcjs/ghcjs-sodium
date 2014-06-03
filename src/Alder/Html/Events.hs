{-# LANGUAGE OverloadedStrings #-}
-- | Events.
module Alder.Html.Events
    ( -- * Keyboard events
      Modifier(..)
    , Location(..)
    , KeyboardEvent(..)
      -- * Form events
    , FormEvent(..)
      -- * Mouse events
    , Button(..)
    , Position(..)
    , MouseEvent(..)
    ) where

import           Prelude             hiding (repeat)

import           Control.Applicative
import           Data.Aeson.Types
import           Data.Char
import           Data.Maybe
import           Data.Set            as Set
import           Data.Text           as Text

import           Alder.Html.Internal

-- | A mouse modifier key.
data Modifier = Alt | Ctrl | Meta | Shift
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

modifiers :: Object -> Parser (Set Modifier)
modifiers ev = Set.fromList . catMaybes <$> sequence
    [ switch Alt   "altKey"
    , switch Ctrl  "ctrlKey"
    , switch Meta  "metaKey"
    , switch Shift "shiftKey"
    ]
  where
    switch a k = (\b -> if b then Just a else Nothing) <$> ev .: k

data Location
    = StandardLocation | LeftLocation | RightLocation | NumpadLocation
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

data KeyboardEvent = KeyboardEvent
    { key               :: !Char
    , keyboardModifiers :: !(Set Modifier)
    , location          :: !Location
    , locale            :: !Text
    , repeat            :: !Bool
    } deriving (Eq, Read, Show)

instance Event KeyboardEvent where
    extractEvent = withObject "keyboard event" $ \ev ->
        KeyboardEvent
            <$> (chr <$> ev .: "key")
            <*> modifiers ev
            <*> (toEnum <$> ev .: "location")
            <*> ev .: "locale"
            <*> ev .: "repeat"

data FormEvent = FormEvent
    { checked :: !Bool
    , value   :: !Text
    }

instance Event FormEvent where
    extractEvent = withObject "form event" $ \ev ->
        FormEvent
            <$> ev .: "checked"
            <*> ev .: "value"

-- | A mouse button.
data Button = LeftButton | MiddleButton | RightButton
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

-- | Coordinate positions.
data Position = Position !Int !Int
    deriving (Eq, Ord, Read, Show)

-- | A mouse event.
data MouseEvent = MouseEvent
    { button         :: !Button
    , mouseModifiers :: !(Set Modifier)
    , clientPosition :: !Position
    , pagePosition   :: !Position
    , screenPosition :: !Position
    } deriving (Eq, Read, Show)

instance Event MouseEvent where
    extractEvent = withObject "mouse event" $ \ev ->
        MouseEvent
            <$> (toEnum <$> ev .: "button")
            <*> modifiers ev
            <*> position ev "clientX" "clientY"
            <*> position ev "pageX" "pageY"
            <*> position ev "screenX" "screenY"
      where
        position ev a b = Position <$> ev .: a <*> ev .: b
