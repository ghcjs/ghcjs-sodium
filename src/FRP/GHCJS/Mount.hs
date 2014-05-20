{-# LANGUAGE OverloadedStrings #-}
-- | Mounting 'Element's on external DOM elements.
module FRP.GHCJS.Mount
    ( mount
    ) where

import           Control.Lens       hiding (children, element)
import           Control.Monad
import           Data.DList         (DList)
import qualified Data.DList         as DList
import           Data.Foldable
import qualified Data.HashSet       as HashSet
import           Data.Monoid
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           FRP.Sodium
import qualified GHCJS.DOM          as DOM
import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.Element  as DOM
import qualified GHCJS.DOM.Node     as DOM

import           FRP.GHCJS.Delta
import           FRP.GHCJS.Element

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
        c  <- DOM.nodeGetFirstChild parent
        void $ foldlM editChild c edits

    editChild c (Insert n) = do
        el <- constructNode n
        _  <- DOM.nodeInsertBefore parent (Just el) c
        return c

    editChild (Just c) (Delete _) = do
        c' <- DOM.nodeGetNextSibling c
        _  <- DOM.nodeRemoveChild parent (Just c)
        return c'

    editChild Nothing (Both d) = do
        el <- constructNode (newValue d)
        _  <- DOM.nodeAppendChild parent (Just el)
        return Nothing

    editChild (Just c) (Both d) = do
        c' <- DOM.nodeGetNextSibling c
        updateNode parent c d
        return c'

    editChild _ _ = return Nothing

-- | Update a 'Node' on a child node.
updateNode :: DOM.Node -> DOM.Node -> Delta Node -> IO ()
updateNode parent child delta =
    case delta ^? match _Parent . equalOn tagName of
        Nothing -> do
            el <- constructNode (newValue delta)
            void $ DOM.nodeReplaceChild parent (Just el) (Just child)
        Just d  -> updateTag child d

-- | Update a 'Tag' on a node.
updateTag :: DOM.Node -> Delta Tag -> IO ()
updateTag node delta = do
    delta ^! slice properties . act (updateProperties node)
    updateElement node (delta ^. slice children)

-- | Update 'Properties' on a node.
updateProperties :: DOM.Node -> Delta Properties -> IO ()
updateProperties node delta = do
    property _id    id          DOM.elementSetId
    property _class toClassList DOM.elementSetClassName
  where
    el = DOM.castToElement node

    property l f m = case delta ^. slice l of
        Delta a b | a == b    -> return ()
                  | otherwise -> m el (f b)

    toClassList = Text.intercalate " " . HashSet.toList

-- | Construct a concrete DOM node from a 'Node'.
constructNode :: Node -> IO DOM.Node
constructNode (Parent tag) = constructTag tag
constructNode (Text s)     = constructText s

-- | Construct a concrete DOM node from a 'Tag'.
constructTag :: Tag -> IO DOM.Node
constructTag tag = do
    Just document <- DOM.currentDocument
    Just el <- DOM.documentCreateElement document (tag ^. tagName)
    let n = DOM.toNode el
    updateTag n (Delta emptyTag tag)
    return n
  where
    emptyTag = tag & properties .~ defaultProperties
                   & children   .~ mempty

-- | Construct a concrete text node.
constructText :: Text -> IO DOM.Node
constructText s = do
    Just document <- DOM.currentDocument
    Just t <- DOM.documentCreateTextNode document s
    return (DOM.toNode t)
