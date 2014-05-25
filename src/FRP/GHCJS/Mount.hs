{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
-- | Mounting 'Element's on external DOM elements.
module FRP.GHCJS.Mount
    ( mount
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State.Class
import           Data.Foldable              (foldlM)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.IORef
import           Data.Text                  (Text)
import qualified Data.Text                  as Text

import           Control.Monad.IOState
import           Data.Delta
import qualified FRP.GHCJS.DOM              as DOM
import           FRP.GHCJS.Input
import           FRP.GHCJS.Internal.Element
import           FRP.GHCJS.Internal.Event
import           FRP.GHCJS.JavaScript

makePrisms ''Element

-- | A node's unique identifier (for this library's purposes).
type Name = Int

-- | The mount state.
data MountState = MountState
    { _mountPoint :: !DOM.Element
    , _model      :: [Element]
    , _nextName   :: !Name
    , _eventMap   :: !(HashMap Name (EventType -> Input DOM.Event))
    }

makeLenses ''MountState

-- | Mount actions.
newtype Mount a = Mount (IOState MountState a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadState MountState)

-- | Run a mount action.
runMount :: Mount a -> IORef MountState -> IO a
runMount (Mount m) = runIOState m

-- | Mount a dynamic list of 'Element's as children of a DOM node. The
-- returned function can be used to push updates to the document.
mount :: DOM.Element -> IO ([Element] -> IO ())
mount e = do
    ref <- newIORef MountState
        { _mountPoint = e
        , _model      = []
        , _nextName   = 0
        , _eventMap   = HashMap.empty
        }
    return $ \es -> runMount (updateModel es) ref

-- | Get the conanical name of an element.
getName :: DOM.Element -> Mount Name
getName e = do
    val <- liftIO $ call e "getAttribute" nameAttr
    case val of
        Nothing -> do
            name <- nextName <<+= 1
            () <- liftIO $ apply e "setAttribute"
                (nameAttr, Text.pack $ show name)
            return name
        Just s  -> return (read $ Text.unpack s)
  where
    nameAttr = "data-ghcjs-sodium-id" :: Text

-- | Register a set of event handlers for an element.
register :: DOM.Element -> (EventType -> Input DOM.Event) -> Mount ()
register e h = do
    name <- getName e
    eventMap . at name ?= h

-- | Unregister event handlers for an element.
unregister :: DOM.Element -> Mount ()
unregister e = do
    name <- getName e
    eventMap . at name .= Nothing

-- | Dispatch a 'DOM.Event' to the appropriate element.
dispatch :: EventType -> DOM.Event -> Mount ()
dispatch evType ev = do
    target <- liftIO $ ev ! "target"
    name <- getName target
    r <- use (eventMap . at name)
    case r of
        Nothing -> return ()
        Just h  -> liftIO $ fire (h evType) ev

-- | Update the model and the DOM tree.
updateModel :: [Element] -> Mount ()
updateModel new = do
    n <- use mountPoint
    old <- model <<.= new
    updateChildren n (Delta old new)

-- | Update an 'Element' on a DOM node. For creation and updates, we modify
-- the tree in the order:
--
-- 1. The underlying tag is created and attached to the parent, if necessary.
-- 2. The children are created/updated.
-- 3. This component is created/updated, inner components to outer ones.
--
-- When an element is destroyed, they are destroyed in the opposite order.
updateElement :: DOM.Element -> DOM.Element -> Delta Element -> Mount ()
updateElement parent e de = case patterns of
    Just m  -> m
    Nothing -> do
        e' <- createElement (newValue de)
        liftIO $ apply parent "replaceChild" (e', e)
  where
    patterns = updateComponent <$> de ^? match _Element . equalOn _1
           <|> updateText      <$> de ^? match _Text

    updateComponent dt = do
        let c = dt ^. slice _2 . to newValue
        unregister e
        register e (handleEvent c)
        liftIO $ create c e
        dt ^! slice _3 . act (updateChildren e)

    updateText (Delta a b)
        | a == b    = return ()
        | otherwise = liftIO $ setProp e "nodeValue" b

-- | Update a list of child 'Element's on a parent DOM node.
updateChildren :: DOM.Element -> Delta [Element] -> Mount ()
updateChildren parent des = editChildren (des ^. diff)
  where
    editChildren edits = do
        c <- liftIO $ parent ! "firstChild"
        void $ foldlM editChild c edits

    editChild c (Insert e) = do
        n <- createElement e
        () <- liftIO $ apply parent "insertBefore" (n, c)
        return c

    editChild (Just c) (Delete e) = do
        c' <- liftIO $ c ! "nextSibling"
        destroyElement c e
        () <- liftIO $ call parent "removeChild" c
        return c'

    editChild Nothing (Both de) = do
        n <- createElement (newValue de)
        () <- liftIO $ call parent "appendChild" n
        return Nothing

    editChild (Just c) (Both de) = do
        c' <- liftIO $ c ! "nextSibling"
        updateElement parent c de
        return c'

    editChild _ _ = return Nothing

-- | Construct a concrete DOM node from an 'Element'.
createElement :: Element -> Mount DOM.Element
createElement (Element tagName c cs) = do
    d <- liftIO $ global "document"
    e <- liftIO $ call d "createElement" tagName
    register e (handleEvent c)
    liftIO $ create c e
    updateChildren e (Delta [] cs)
    return e
createElement (Text s) = do
    d <- liftIO $ global "document"
    liftIO $ call d "createTextNode" s

-- | Destroy an 'Element'.
destroyElement :: DOM.Element -> Element -> Mount ()
destroyElement e (Element _ c cs) = do
    updateChildren e (Delta cs [])
    liftIO $ destroy c e
    unregister e
destroyElement _ _ = return ()
