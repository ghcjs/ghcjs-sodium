{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module FRP.GHCJS.Dispatch
    ( Dispatch
    , HasDispatch(..)
    , Dispatches
    , register
    , unregister
    , dispatchEvent
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.State.Class
import           Data.HashMap.Strict       (HashMap)
import           GHC.Generics
import qualified GHCJS.DOM.Element         as DOM
import qualified GHCJS.DOM.Event           as DOM
import           GHCJS.Types

import           Data.Default
import           FRP.GHCJS.Events

-- | A node's unique identifier (for this library's purposes).
type Name = Int

-- | The event dispatch system state.
data Dispatch = Dispatch
    { _nextName :: !Name
    , _eventMap :: !(HashMap Name Events)
    } deriving (Generic)

makeClassy ''Dispatch

instance Default Dispatch

-- | Monads that can access the dispatch state.
class (MonadIO m, MonadState s m, HasDispatch s) => Dispatches s m

-- | Get the conanical name of an element.
getName :: Dispatches s m => DOM.Element -> m Name
getName e = do
    hasNameAttr <- liftIO $ DOM.elementHasAttribute e nameAttr
    if hasNameAttr
        then liftIO $ read <$> DOM.elementGetAttribute e nameAttr
        else do
            name <- nextName <<+= 1
            liftIO $ DOM.elementSetAttribute e nameAttr (show name)
            return name
  where
    nameAttr = "data-ghcjs-sodium-id" :: JSString

-- | Register a set of event handlers for an element, overwriting any
-- previous handlers.
register :: Dispatches s m => DOM.Element -> Events -> m ()
register e evs = do
    name <- getName e
    eventMap . at name ?= evs

-- | Unregister an element.
unregister :: Dispatches s m => DOM.Element -> m ()
unregister e = do
    name <- getName e
    eventMap . at name .= Nothing

-- | Dispatch a 'DOM.Event' with a type name to the appropriate element.
dispatchEvent :: Dispatches s m => JSString -> DOM.Event -> m ()
dispatchEvent = undefined
