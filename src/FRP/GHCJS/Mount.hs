{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
-- | Mounting 'Element's on external DOM elements.
module FRP.GHCJS.Mount
    ( mount
    ) where

import           Control.Applicative
import           Control.Lens        hiding (children, element)
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
updateNode parent child delta = case patterns of
    Just m  -> m
    Nothing -> do
        n <- createNode (newValue delta)
        void $ DOM.nodeReplaceChild parent (Just n) (Just child)
  where
    patterns = updateComponent child <$> delta ^? match _Parent . equalOn name
           <|> updateText      child <$> delta ^? match _Text

-- | Update a 'Tag' on a node.
updateComponent :: DOM.Node -> Delta Component -> IO ()
updateComponent n delta = do
    update (newValue delta) n
    updateElement n (children <$> delta)

-- | Update a text node.
updateText :: DOM.Node -> Delta Text -> IO ()
updateText n (Delta a b)
    | a == b    = return ()
    | otherwise = DOM.nodeSetNodeValue n b

-- | Construct a concrete DOM node from a 'Node'.
createNode :: Node -> IO DOM.Node
createNode (Parent c) = create c
createNode (Text s)   = createText s

-- | Construct a concrete text node.
createText :: Text -> IO DOM.Node
createText s = do
    Just document <- DOM.currentDocument
    Just t <- DOM.documentCreateTextNode document s
    return (DOM.toNode t)

-- | Delete a 'Node'.
deleteNode :: DOM.Node -> Node -> IO ()
deleteNode n (Parent c) = delete c n
deleteNode _ _          = return ()
