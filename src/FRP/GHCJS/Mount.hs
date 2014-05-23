{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
-- | Mounting 'Element's on external DOM elements.
module FRP.GHCJS.Mount
    ( -- * Elements
      Element(..)
    , Component(..)
      -- * Mounting
    , Mount
    , mount
    ) where

import           Control.Applicative
import           Control.Lens           hiding (children)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable          (foldlM)
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HashMap
import           Data.IORef
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           FRP.Sodium
import           GHCJS.DOM.Document
import           GHCJS.DOM.Node

import           Control.Monad.IOState
import           FRP.GHCJS.Delta
import           FRP.GHCJS.Input

-- | A document element.
data Element
      -- | Extend an 'Element' with initialization and update operations.
    = Extend Component Element
      -- | A vanilla HTML tag.
    | Tag !Text [Element]
      -- | A text node.
    | Text !Text

-- | A logical component in the document.
data Component = Component
    { -- | A component name that uniquely identifies the type or class of
      -- this component. 'update' and 'delete' may assume that the 'Node'
      -- has been created by 'create' of the same component name.
      componentName :: !Text
      -- | Create the component.
    , create        :: Node -> Mount ()
      -- | Update an existing DOM node for this component.
    , update        :: Node -> Mount ()
      -- | Delete the component, performing any cleanup.
    , destroy       :: Node -> Mount ()
    }

-- | A state monad for mounting.
type Mount = IOState MountState

-- | The mount state.
data MountState = MountState
    { _document :: !Document
    , _nextId   :: !NodeId
    , _nodes    :: !(HashMap NodeId Node)
    , _handlers :: !(HashMap NodeId Inputs)
    }

-- | A node's unique identifier (for this library's purposes).
type NodeId = Int

makeLenses ''MountState
makePrisms ''Element

-- | Given a starting value, bundle the old and new values into a 'Delta'
-- when the 'Event' fires.
deltas :: a -> Event a -> Reactive (Event (Delta a))
deltas z e = do
    b <- hold z e
    return $ snapshot (flip Delta) e b

-- | Create a new 'MountState' for a mount point.
newMountState :: Node -> IO MountState
newMountState n = do
    Just d <- nodeGetOwnerDocument n
    return MountState
        { _document = d
        , _nextId   = 0
        , _nodes    = HashMap.empty
        , _handlers = HashMap.empty
        }

-- | Mount a dynamic list of 'Element's as children of a DOM 'Node'.
-- The returned action will stop updating the DOM.
mount :: IsNode e => e -> Behavior [Element] -> IO (IO ())
mount parent b = do
    let n = toNode parent
    s   <- newMountState n
    ref <- newIORef s
    sync $ do
        e <- deltas [] (value b)
        listen e (\des -> runIOState (updateChildren n des) ref)

-- | Update an 'Element' on a DOM node. For creation and updates, we modify
-- the tree in the order:
--
-- 1. The underlying tag is created and attached to the parent, if necessary.
-- 2. The children are created/updated.
-- 3. This component is created/updated, inner components to outer ones.
--
-- When an element is destroyed, they are destroyed in the opposite order.
updateElement :: Node -> Node -> Delta Element -> Mount ()
updateElement parent n de = case patterns of
    Just m  -> m
    Nothing -> do
        n' <- createElement (newValue de)
        void . liftIO $ nodeReplaceChild parent (Just n') (Just n)
  where
    patterns = updateComponent <$> de ^? match _Extend . equalOn (_1 . to componentName)
           <|> updateTag       <$> de ^? match _Tag . equalOn _1
           <|> updateText      <$> de ^? match _Text

    updateComponent dt = do
        updateElement parent n (dt ^. slice _2)
        update (dt ^. slice _1 . to newValue) n

    updateTag dt = dt ^! slice _2 . act (updateChildren n)

    updateText (Delta a b)
        | a == b    = return ()
        | otherwise = liftIO $ nodeSetNodeValue n b

-- | Update a list of child 'Element's on a parent DOM 'Node'.
updateChildren :: Node -> Delta [Element] -> Mount ()
updateChildren parent des = editChildren (des ^. diff)
  where
    editChildren edits = do
        c <- liftIO $ nodeGetFirstChild parent
        void $ foldlM editChild c edits

    editChild c (Insert e) = do
        n <- createElement e
        _ <- liftIO $ nodeInsertBefore parent (Just n) c
        return c

    editChild (Just c) (Delete e) = do
        c' <- liftIO $ nodeGetNextSibling c
        destroyElement c e
        _ <- liftIO $ nodeRemoveChild parent (Just c)
        return c'

    editChild Nothing (Both de) = do
        n <- createElement (newValue de)
        _ <- liftIO $ nodeAppendChild parent (Just n)
        return Nothing

    editChild (Just c) (Both de) = do
        c' <- liftIO $ nodeGetNextSibling c
        updateElement parent c de
        return c'

    editChild _ _ = return Nothing

-- | Construct a concrete DOM node from an 'Element'.
createElement :: Element -> Mount Node
createElement (Extend c e) = do
    n <- createElement e
    create c n
    return n
createElement (Tag s cs)   = do
    d <- use document
    Just t <- liftIO $ documentCreateElement d s
    let n = toNode t
    updateChildren n (Delta [] cs)
    return n
createElement (Text s)     = do
    d <- use document
    Just t <- liftIO $ documentCreateTextNode d s
    return (toNode t)

-- | Destroy an 'Element'.
destroyElement :: Node -> Element -> Mount ()
destroyElement n (Extend c e) = do
    destroy c n
    destroyElement n e
destroyElement n (Tag _ cs)   = updateChildren n (Delta cs [])
destroyElement _ _            = return ()
