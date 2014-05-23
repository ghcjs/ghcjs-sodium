{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
-- | Mounting 'Element's on external DOM elements.
module FRP.GHCJS.Mount
    ( -- * Elements
      Element(..)
    , Component(..)
      -- * Mounting
    , Mount
    , mount
    , updateInputs
    ) where

import           Control.Applicative
import           Control.Lens              hiding (children)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State.Class
import           Data.Foldable             (foldlM)
import           Data.IORef
import           Data.Text                 (Text)
import           FRP.Sodium
import qualified GHCJS.DOM.Document        as DOM
import qualified GHCJS.DOM.Element         as DOM
import qualified GHCJS.DOM.Node            as DOM
import           GHCJS.Types
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

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
    , create        :: DOM.Element -> Mount ()
      -- | Update an existing DOM node for this component.
    , update        :: DOM.Element -> Mount ()
      -- | Delete the component, performing any cleanup.
    , destroy       :: DOM.Element -> Mount ()
    }

-- | A state monad for mounting.
newtype Mount a = Mount { runMount :: IOState MountState a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState MountState)

-- | The mount state.
data MountState = MountState
    { _document :: !DOM.Document
    , _nextName :: !Name
    , _inputs   :: !(HashMap Name Inputs)
    }

-- | A node's unique identifier (for this library's purposes).
type Name = Int

makeLenses ''MountState
makePrisms ''Element

-- | Given a starting value, bundle the old and new values into a 'Delta'
-- when the 'Event' fires.
deltas :: a -> Event a -> Reactive (Event (Delta a))
deltas z e = do
    b <- hold z e
    return $ snapshot (flip Delta) e b

-- | Create a new 'MountState' for a mount point.
newMountState :: DOM.Node -> IO MountState
newMountState n = do
    Just d <- DOM.nodeGetOwnerDocument n
    return MountState
        { _document = d
        , _nextName = 0
        , _inputs   = HashMap.empty
        }

-- | Mount a dynamic list of 'Element's as children of a DOM 'Node'.
-- The returned action will stop updating the DOM.
mount :: DOM.IsNode e => e -> Behavior [Element] -> IO (IO ())
mount parent b = do
    let n = DOM.toNode parent
    s   <- newMountState n
    ref <- newIORef s
    sync $ do
        e <- deltas [] (value b)
        listen e $ \des -> runIOState (runMount $ updateChildren n des) ref

-- | Get the name of an element.
getName :: DOM.Element -> Mount Name
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

-- | Update the 'Inputs' for an 'DOM.Element'.
updateInputs :: DOM.IsElement e => e -> Inputs -> Mount ()
updateInputs e is = do
    name <- getName (DOM.toElement e)
    inputs . at name ?= is

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
        update (dt ^. slice _1 . to newValue) (DOM.castToElement n)

    updateTag dt = dt ^! slice _2 . act (updateChildren n)

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
    create c (DOM.castToElement n)
    return n
createElement (Tag s cs)   = do
    d <- use document
    Just e <- liftIO $ DOM.documentCreateElement d s
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
    destroy c (DOM.castToElement n)
    destroyElement n e
destroyElement n (Tag _ cs)   = updateChildren n (Delta cs [])
destroyElement _ _            = return ()
