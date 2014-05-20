{-# LANGUAGE OverloadedStrings #-}
-- | Mounting 'Delta's on external DOM elements.
module FRP.GHCJS.Mount
    ( mountElement
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
import qualified GHCJS.DOM          as DOM
import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.Element  as DOM
import qualified GHCJS.DOM.Node     as DOM

import           FRP.GHCJS.Diff
import           FRP.GHCJS.Element

-- | A 'DList' is isomorphic to a list.
dlist :: Iso [a] [b] (DList a) (DList b)
dlist = iso DList.fromList DList.toList

-- | Mount an 'Element' on a parent DOM node.
mountElement :: DOM.Node -> Delta Element -> IO ()
mountElement parent delta =
    delta ^! slice (from element . from dlist) . diff . act editChildren
  where
    editChildren edits = do
        c  <- DOM.nodeGetFirstChild parent
        void $ foldlM editChild c edits

    editChild c (Insert n) = do
        el <- constructNode n
        DOM.nodeInsertBefore parent c (Just el)
        return c

    editChild (Just c) (Delete _) = do
        c' <- DOM.nodeGetNextSibling c
        DOM.nodeRemoveChild parent (Just c)
        return c'

    editChild Nothing (Both d) = do
        el <- constructNode (newValue d)
        DOM.nodeAppendChild parent (Just el)
        return Nothing

    editChild (Just c) (Both d) = do
        c' <- DOM.nodeGetNextSibling c
        mountNode parent c d
        return c'

    editChild _ _ = return Nothing

-- | Mount a 'Node' on a child node.
mountNode :: DOM.Node -> DOM.Node -> Delta Node -> IO ()
mountNode parent child delta =
    case delta ^? match _Parent . equalOn tagName of
        Nothing -> do
            el <- constructNode (newValue delta)
            void $ DOM.nodeReplaceChild parent (Just child) (Just el)
        Just d  -> mountTag child d

-- | Mount a 'Tag' on a node.
mountTag :: DOM.Node -> Delta Tag -> IO ()
mountTag node delta = do
    delta ^! slice properties . act (mountProperties node)
    mountElement node (delta ^. slice children)

-- | Mount 'Properties' on a node.
mountProperties :: DOM.Node -> Delta Properties -> IO ()
mountProperties node delta = do
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
    mountTag n (Delta emptyTag tag)
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
