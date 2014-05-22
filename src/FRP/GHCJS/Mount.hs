-- | Mounting 'Element's on external DOM elements.
module FRP.GHCJS.Mount
    ( mount
    ) where

import           Control.Applicative
import           Control.Lens        hiding (children)
import           Control.Monad
import           Data.Foldable       (foldlM)
import           FRP.Sodium
import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.Node

import           FRP.GHCJS.Delta
import           FRP.GHCJS.Types

-- | Given a starting value, bundle the old and new values into a 'Delta'
-- when the 'Event' fires.
deltas :: a -> Event a -> Reactive (Event (Delta a))
deltas z e = do
    b <- hold z e
    return $ snapshot (flip Delta) e b

-- | Mount a dynamic list of 'Element's as children of a DOM 'Node'.
-- The returned action will stop updating the DOM.
mount :: IsNode e => e -> Behavior [Element] -> Reactive (IO ())
mount parent b = do
    e <- deltas [] (value b)
    listen e (updateChildren (toNode parent))

-- | Update an 'Element' on a DOM node. For creation and updates, we modify
-- the tree in the order:
--
-- 1. The underlying tag is created and attached to the parent, if necessary.
-- 2. The children are created/updated.
-- 3. This component is created/updated, inner components to outer ones.
--
-- When an element is destroyed, they are destroyed in the opposite order.
updateElement :: Node -> Node -> Delta Element -> IO ()
updateElement parent n de = case patterns of
    Just m  -> m
    Nothing -> do
        n' <- createElement (newValue de)
        void $ nodeReplaceChild parent (Just n') (Just n)
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
        | otherwise = nodeSetNodeValue n b

-- | Update a list of child 'Element's on a parent DOM 'Node'.
updateChildren :: Node -> Delta [Element] -> IO ()
updateChildren parent des = editChildren (des ^. diff)
  where
    editChildren edits = do
        c <- nodeGetFirstChild parent
        void $ foldlM editChild c edits

    editChild c (Insert e) = do
        n <- createElement e
        _ <- nodeInsertBefore parent (Just n) c
        return c

    editChild (Just c) (Delete e) = do
        c' <- nodeGetNextSibling c
        destroyElement c e
        _ <- nodeRemoveChild parent (Just c)
        return c'

    editChild Nothing (Both de) = do
        n <- createElement (newValue de)
        _ <- nodeAppendChild parent (Just n)
        return Nothing

    editChild (Just c) (Both de) = do
        c' <- nodeGetNextSibling c
        updateElement parent c de
        return c'

    editChild _ _ = return Nothing

-- | Construct a concrete DOM node from an 'Element'.
createElement :: Element -> IO Node
createElement (Extend c e) = do
    n <- createElement e
    create c n
    return n
createElement (Tag s cs)   = do
    Just document <- currentDocument
    Just t <- documentCreateElement document s
    let n = toNode t
    updateChildren n (Delta [] cs)
    return n
createElement (Text s)     = do
    Just document <- currentDocument
    Just t <- documentCreateTextNode document s
    return (toNode t)

-- | Destroy an 'Element'.
destroyElement :: Node -> Element -> IO ()
destroyElement n (Extend c e) = do
    destroy c n
    destroyElement n e
destroyElement n (Tag _ cs)   = updateChildren n (Delta cs [])
destroyElement _ _            = return ()
