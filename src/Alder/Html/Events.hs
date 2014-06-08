{-# LANGUAGE OverloadedStrings #-}
-- | Events.
module Alder.Html.Events
    ( -- * Types
      Modifier(..)
    , Location(..)
    , Button(..)
    , Position(..)
      -- * Events
    , KeyboardEvent(..)
    , FocusEvent(..)
    , InputEvent(..)
    , SubmitEvent(..)
    , MouseEvent(..)
    ) where

import           Prelude             hiding (repeat)

import           Control.Applicative
import           Data.Char
import           Data.Maybe
import           Data.Set            as Set
import           Data.Text           as Text

import           Alder.Html.Internal
import           Alder.JavaScript

-- | A mouse modifier key.
data Modifier = Alt | Ctrl | Meta | Shift
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

data Location
    = StandardLocation | LeftLocation | RightLocation | NumpadLocation
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

-- | A mouse button.
data Button = LeftButton | MiddleButton | RightButton
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

-- | Coordinate positions.
data Position = Position !Int !Int
    deriving (Eq, Ord, Read, Show)

modifiers :: JSObj -> IO (Set Modifier)
modifiers ev = Set.fromList . catMaybes <$> sequence
    [ switch Alt   "altKey"
    , switch Ctrl  "ctrlKey"
    , switch Meta  "metaKey"
    , switch Shift "shiftKey"
    ]
  where
    switch a k = (\b -> if b then Just a else Nothing) <$> ev .: k

data KeyboardEvent = KeyboardEvent
    { key               :: !Char
    , keyboardModifiers :: !(Set Modifier)
    , location          :: !Location
    , locale            :: !(Maybe Text)
    , repeat            :: !Bool
    } deriving (Eq, Read, Show)

instance Event KeyboardEvent where
    extractEvent ev = KeyboardEvent
        <$> (alt <$> ev .: "keyCode" <*> ev .: "charCode")
        <*> modifiers ev
        <*> (toEnum <$> ev .: "location")
        <*> ev .: "locale"
        <*> ev .: "repeat"
      where
        alt 0 b = chr b
        alt a _ = chr a

data InputEvent = InputEvent
    { checked :: !Bool
    , value   :: !Text
    } deriving (Eq, Read, Show)

instance Event InputEvent where
    extractEvent ev = do
        target <- ev .: "target"
        InputEvent
            <$> target .: "checked"
            <*> target .: "value"

data FocusEvent = FocusEvent deriving (Eq, Read, Show)

instance Event FocusEvent where
    extractEvent _ = return FocusEvent

data SubmitEvent = SubmitEvent deriving (Eq, Read, Show)

instance Event SubmitEvent where
    extractEvent _ = return SubmitEvent

-- | A mouse event.
data MouseEvent = MouseEvent
    { button         :: !Button
    , mouseModifiers :: !(Set Modifier)
    , clientPosition :: !Position
    , pagePosition   :: !Position
    , screenPosition :: !Position
    } deriving (Eq, Read, Show)

instance Event MouseEvent where
    extractEvent ev = MouseEvent
        <$> (toEnum <$> ev .: "button")
        <*> modifiers ev
        <*> position "clientX" "clientY"
        <*> position "pageX" "pageY"
        <*> position "screenX" "screenY"
      where
        position a b = Position <$> ev .: a <*> ev .: b
