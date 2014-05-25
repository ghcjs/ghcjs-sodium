{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
-- | Mounting 'Element's on external DOM elements.
module FRP.GHCJS.Mount
    ( mount
    ) where

import           Control.Applicative
import           Control.Lens               hiding (children)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State.Class
import           Data.Foldable              (foldlM)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.IORef
import           Data.Text                  (Text)
import qualified GHCJS.DOM.Document         as DOM
import qualified GHCJS.DOM.Element          as DOM
import qualified GHCJS.DOM.Event            as DOM
import qualified GHCJS.DOM.Node             as DOM

import           Control.Monad.IOState
import           Data.Delta
import           FRP.GHCJS.Input
import           FRP.GHCJS.Internal.Element
import           FRP.GHCJS.Internal.Event

makePrisms ''Element

-- | A node's unique identifier (for this library's purposes).
type Name = Int

-- | The mount state.
data MountState = MountState
    { _document   :: !DOM.Document
    , _mountPoint :: !DOM.Node
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

-- | Mount a dynamic list of 'Element's as children of a DOM 'Node'. The
-- returned function can be used to push updates to the document.
mount :: DOM.IsNode a => a -> IO ([Element] -> IO ())
mount parent = do
    let n = DOM.toNode parent
    Just doc <- DOM.nodeGetOwnerDocument n
    ref <- newIORef MountState
        { _document   = doc
        , _mountPoint = n
        , _model      = []
        , _nextName   = 0
        , _eventMap   = HashMap.empty
        }
    return $ \es -> runMount (updateModel es) ref

-- | Get the conanical name of an element.
getName :: DOM.IsElement a => a -> Mount Name
getName e = do
    hasName <- liftIO $ DOM.elementHasAttribute e nameAttr
    if hasName
        then liftIO $ read <$> DOM.elementGetAttribute e nameAttr
        else do
            name <- nextName <<+= 1
            liftIO $ DOM.elementSetAttribute e nameAttr (show name)
            return name
  where
    nameAttr = "data-ghcjs-sodium-id" :: Text

-- | Register a set of event handlers for an element, overwriting any
-- previous handlers.
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
    Just target <- liftIO $ DOM.eventGetTarget ev
    name <- getName (DOM.castToElement target)
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
updateElement :: DOM.Node -> DOM.Node -> Delta Element -> Mount ()
updateElement parent n de = case patterns of
    Just m  -> m
    Nothing -> do
        n' <- createElement (newValue de)
        void . liftIO $ DOM.nodeReplaceChild parent (Just n') (Just n)
  where
    patterns = updateComponent <$> de ^? match _Extend . equalOn (_1 . to componentName)
           <|> updateTag       <$> de ^? match _Tag . equalOn _1
           <|> updateText      <$> de ^? match _Text

    updateComponent dt = do
        updateElement parent n (dt ^. slice _2)
        liftIO $ update (dt ^. slice _1 . to newValue) (DOM.castToElement n)

    updateTag dt = dt ^! slice _3 . act (updateChildren n)

    updateText (Delta a b)
        | a == b    = return ()
        | otherwise = liftIO $ DOM.nodeSetNodeValue n b

-- | Update a list of child 'Element's on a parent DOM 'Node'.
updateChildren :: DOM.Node -> Delta [Element] -> Mount ()
updateChildren parent des = editChildren (des ^. diff)
  where
    editChildren edits = do
        c <- liftIO $ DOM.nodeGetFirstChild parent
        void $ foldlM editChild c edits

    editChild c (Insert e) = do
        n <- createElement e
        _ <- liftIO $ DOM.nodeInsertBefore parent (Just n) c
        return c

    editChild (Just c) (Delete e) = do
        c' <- liftIO $ DOM.nodeGetNextSibling c
        destroyElement c e
        _ <- liftIO $ DOM.nodeRemoveChild parent (Just c)
        return c'

    editChild Nothing (Both de) = do
        n <- createElement (newValue de)
        _ <- liftIO $ DOM.nodeAppendChild parent (Just n)
        return Nothing

    editChild (Just c) (Both de) = do
        c' <- liftIO $ DOM.nodeGetNextSibling c
        updateElement parent c de
        return c'

    editChild _ _ = return Nothing

-- | Construct a concrete DOM node from an 'Element'.
createElement :: Element -> Mount DOM.Node
createElement (Extend c e) = do
    n <- createElement e
    liftIO $ create c (DOM.castToElement n)
    return n
createElement (Tag s h cs) = do
    d <- use document
    Just e <- liftIO $ DOM.documentCreateElement d s
    register e h
    let n = DOM.toNode e
    updateChildren n (Delta [] cs)
    return n
createElement (Text s)     = do
    d <- use document
    Just t <- liftIO $ DOM.documentCreateTextNode d s
    return (DOM.toNode t)

-- | Destroy an 'Element'.
destroyElement :: DOM.Node -> Element -> Mount ()
destroyElement n (Extend c e) = do
    liftIO $ destroy c (DOM.castToElement n)
    destroyElement n e
destroyElement n (Tag _ _ cs) = do
    updateChildren n (Delta cs [])
    unregister (DOM.castToElement n)
destroyElement _ _            = return ()
