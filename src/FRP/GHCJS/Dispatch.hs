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
import           Control.Monad
import           Control.Monad.State.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HashMap
import           Data.Text                 (Text)
import           FRP.Sodium                (sync)
import           GHC.Generics
import qualified GHCJS.DOM.Element         as DOM
import qualified GHCJS.DOM.Event           as DOM
import           GHCJS.Types

import           Data.Default
import qualified FRP.GHCJS.Events          as E
import           FRP.GHCJS.Input
import           FRP.GHCJS.Internal.Events

-- | A node's unique identifier (for this library's purposes).
type Name = Int

-- | The event dispatch system state.
data Dispatch = Dispatch
    { _nextName :: !Name
    , _eventMap :: !(HashMap Name E.Events)
    } deriving (Generic)

makeClassy ''Dispatch

instance Default Dispatch

-- | Monads that can access the dispatch state.
class (Functor m, MonadIO m, MonadState s m, HasDispatch s) => Dispatches s m

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
register :: Dispatches s m => DOM.Element -> E.Events -> m ()
register e evs = do
    name <- getName e
    eventMap . at name ?= evs

-- | Unregister an element.
unregister :: Dispatches s m => DOM.Element -> m ()
unregister e = do
    name <- getName e
    eventMap . at name .= Nothing

-- | Dispatch a 'DOM.Event' with a type name to the appropriate element.
dispatchEvent :: Dispatches s m => Text -> DOM.Event -> m ()
dispatchEvent evType ev = void . runMaybeT $ do
    target <- MaybeT . liftIO $ DOM.eventGetTarget ev
    name   <- lift $ getName (DOM.castToElement target)
    is     <- MaybeT $ use (eventMap . at name)
    action <- MaybeT . return $ HashMap.lookup evType selectors
    liftIO $ action is ev

-- | Select an event to fire.
select
    :: Event e
    => (Getting (Input e) a (Input e))
    -> a
    -> DOM.Event
    -> IO ()
select l a ev = do
    e <- extractEvent ev
    sync $ fire (a ^. l) e

-- | Event selectors.
selectors :: HashMap Text (E.Events -> DOM.Event -> IO ())
selectors = HashMap.fromList
    [ "click"       ==> select E.click
    , "doubleClick" ==> select E.doubleClick
    , "drag"        ==> select E.drag
    , "dragEnd"     ==> select E.dragEnd
    , "dragEnter"   ==> select E.dragEnter
    , "dragExit"    ==> select E.dragExit
    , "dragLeave"   ==> select E.dragLeave
    , "dragOver"    ==> select E.dragOver
    , "dragStart"   ==> select E.dragStart
    , "drop"        ==> select E.drop
    , "mouseDown"   ==> select E.mouseDown
    , "mouseEnter"  ==> select E.mouseEnter
    , "mouseLeave"  ==> select E.mouseLeave
    , "mouseMove"   ==> select E.mouseMove
    , "mouseOut"    ==> select E.mouseOut
    , "mouseOver"   ==> select E.mouseOver
    , "mouseUp"     ==> select E.mouseUp
    ]
  where
    (==>) = (,)
