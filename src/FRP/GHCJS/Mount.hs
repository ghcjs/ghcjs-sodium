{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
-- | Mounting 'Element's on external DOM elements.
module FRP.GHCJS.Mount
    ( mount
    ) where

import           Control.Applicative
import           Control.Lens        hiding (element)
import           Control.Monad
import           Data.DList          (DList)
import qualified Data.DList          as DList
import           Data.Foldable
import           Data.Monoid
import           Data.Text           (Text)
import           FRP.Sodium
import qualified GHCJS.DOM           as DOM
import qualified GHCJS.DOM.Document  as DOM
import qualified GHCJS.DOM.Node      as DOM

import           FRP.GHCJS.Delta
import           FRP.GHCJS.Types

makeIso    ''Element
makePrisms ''Node

-- | Given a starting value, bundle the old and new values into a 'Delta'
-- when the 'Event' fires.
deltas :: a -> Event a -> Reactive (Event (Delta a))
deltas z e = do
    b <- hold z e
    return $ snapshot (flip Delta) e b

-- | Mount a dynamic 'Element' on a parent node. The returned action will
-- stop updating the element.
mount :: DOM.IsNode e => e -> Behavior Element -> Reactive (IO ())
mount parent b = do
    e <- deltas mempty (value b)
    listen e (updateElement (DOM.toNode parent))

-- | A 'DList' is isomorphic to a list.
dlist :: Iso [a] [b] (DList a) (DList b)
dlist = iso DList.fromList DList.toList

-- | Update an 'Element' on a parent DOM node.
updateElement :: DOM.Node -> Delta Element -> IO ()
updateElement parent delta =
    delta ^! slice (from element . from dlist) . diff . act editChildren
  where
    editChildren edits = do
        c <- DOM.nodeGetFirstChild parent
        void $ foldlM editChild c edits

    editChild c (Insert x) = do
        n <- createNode x
        _ <- DOM.nodeInsertBefore parent (Just n) c
        return c

    editChild (Just c) (Delete x) = do
        c' <- DOM.nodeGetNextSibling c
        deleteNode c x
        _ <- DOM.nodeRemoveChild parent (Just c)
        return c'

    editChild Nothing (Both d) = do
        n <- createNode (newValue d)
        _ <- DOM.nodeAppendChild parent (Just n)
        return Nothing

    editChild (Just c) (Both d) = do
        c' <- DOM.nodeGetNextSibling c
        updateNode parent c d
        return c'

    editChild _ _ = return Nothing

-- | Update a 'Node' on a child node.
updateNode :: DOM.Node -> DOM.Node -> Delta Node -> IO ()
updateNode parent n delta = case patterns of
    Just m  -> m
    Nothing -> do
        n' <- createNode (newValue delta)
        void $ DOM.nodeReplaceChild parent (Just n') (Just n)
  where
    patterns = updateTag    <$> delta ^? match _Tag . equalOn (name . fst)
           <|> updateText n <$> delta ^? match _Text

    updateTag d = updateComponent n (fst <$> d) (snd <$> d)

-- | Update a 'Component'.
updateComponent :: DOM.Node -> Delta Component -> Delta Element -> IO ()
updateComponent n delta cs = do
    updateElement n cs
    update (newValue delta) n

-- | Update a text node.
updateText :: DOM.Node -> Delta Text -> IO ()
updateText n (Delta a b)
    | a == b    = return ()
    | otherwise = DOM.nodeSetNodeValue n b

-- | Construct a concrete DOM node from a 'Node'.
createNode :: Node -> IO DOM.Node
createNode (Tag c cs) = createComponent c cs
createNode (Text s)   = createText s

-- | Construct a 'Component'.
createComponent :: Component -> Element -> IO DOM.Node
createComponent c cs = do
    Just document <- DOM.currentDocument
    Just e <- DOM.documentCreateElement document (tagName c)
    let n = DOM.toNode e
    updateElement n (Delta mempty cs)
    create c n
    return n

-- | Construct a concrete text node.
createText :: Text -> IO DOM.Node
createText s = do
    Just document <- DOM.currentDocument
    Just t <- DOM.documentCreateTextNode document s
    return (DOM.toNode t)

-- | Delete a 'Node'.
deleteNode :: DOM.Node -> Node -> IO ()
deleteNode n (Tag c cs) = deleteComponent n c cs
deleteNode _ _          = return ()

-- | Delete a 'Component'.
deleteComponent :: DOM.Node -> Component -> Element -> IO ()
deleteComponent n c cs = do
    delete c n
    cs ^! from element . folded . act (deleteNode n)
