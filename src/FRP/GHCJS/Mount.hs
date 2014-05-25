{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
-- | Mounting 'Element's on external DOM elements.
module FRP.GHCJS.Mount
    ( mount
    ) where

import           Control.Applicative
import           Control.Lens                  hiding (children)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State.Class
import           Data.Foldable                 (foldlM)
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as HashMap
import           Data.IORef
import           Data.Text                     (Text)
import qualified GHCJS.DOM.Document            as DOM
import qualified GHCJS.DOM.Element             as DOM
import qualified GHCJS.DOM.Event               as DOM
import qualified GHCJS.DOM.EventTargetClosures as DOM
import qualified GHCJS.DOM.Node                as DOM

import           Control.Monad.IOState
import           Data.Delta
import qualified FRP.GHCJS.Events              as E
import           FRP.GHCJS.Input
import           FRP.GHCJS.Internal.Element
import           FRP.GHCJS.Internal.Events

-- | A node's unique identifier (for this library's purposes).
type Name = Int

-- | The mount state.
data MountState = MountState
    { _document :: !DOM.Document
    , _model    :: [Element]
    , _nextName :: !Name
    , _eventMap :: !(HashMap Name E.Events)
    }

makeLenses ''MountState

-- | A state monad for mounting.
newtype Mount a = Mount (IOState MountState a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadState MountState)

-- | Run the 'Mount' monad.
runMount :: Mount a -> IORef MountState -> IO a
runMount (Mount m) = runIOState m

-- | Mount a dynamic list of 'Element's as children of a DOM 'Node'. The
-- returned function can be used to push updates to the document.
mount :: DOM.IsNode e => e -> IO ([Element] -> IO ())
mount parent = do
    let n = DOM.toNode parent
    Just doc <- DOM.nodeGetOwnerDocument n
    ref <- newIORef MountState
        { _document = doc
        , _model    = []
        , _nextName = 0
        , _eventMap = HashMap.empty
        }
    trapEvents doc ref
    return $ \es -> runMount (updateModel n es) ref

-- | Get the conanical name of an element.
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
    nameAttr = "data-ghcjs-sodium-id" :: Text

-- | Register a set of event handlers for an element, overwriting any
-- previous handlers.
register :: DOM.Element -> E.Events -> Mount ()
register e evs = do
    name <- getName e
    eventMap . at name ?= evs

-- | Unregister an element.
unregister :: DOM.Element -> Mount ()
unregister e = do
    name <- getName e
    eventMap . at name .= Nothing

-- | Listen to a top-level event.
listen :: DOM.Document -> Text -> (DOM.Event -> IO ()) -> IO ()
listen doc evType f =
    void . DOM.eventTargetAddEventListener doc evType False $ \_ -> f

-- | Set up event handlers.
trapEvents :: DOM.Document -> IORef MountState -> IO ()
trapEvents doc ref = do
    trap "click"       E.click
    trap "doubleClick" E.doubleClick
    trap "drag"        E.drag
    trap "dragEnd"     E.dragEnd
    trap "dragEnter"   E.dragEnter
    trap "dragExit"    E.dragExit
    trap "dragLeave"   E.dragLeave
    trap "dragOver"    E.dragOver
    trap "dragStart"   E.dragStart
    trap "drop"        E.drop
    trap "mouseDown"   E.mouseDown
    trap "mouseEnter"  E.mouseEnter
    trap "mouseLeave"  E.mouseLeave
    trap "mouseMove"   E.mouseMove
    trap "mouseOut"    E.mouseOut
    trap "mouseOver"   E.mouseOver
    trap "mouseUp"     E.mouseUp
  where
    trap evType l = listen doc evType $ \ev -> runMount (dispatch l ev) ref

-- | Dispatch a 'DOM.Event' to the appropriate element.
dispatch :: Event e => Getting (Input e) E.Events (Input e) -> DOM.Event -> Mount ()
dispatch l ev = do
    Just target <- liftIO $ DOM.eventGetTarget ev
    name <- getName (DOM.castToElement target)
    r <- use (eventMap . at name)
    case r of
        Nothing -> return ()
        Just es -> liftIO $ do
            e <- extractEvent ev
            DOM.eventStopPropagation ev
            fire (es ^. l) e

-- | Update the model and the DOM tree.
updateModel :: DOM.Node -> [Element] -> Mount ()
updateModel n new = do
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
createElement (Extend c e)   = do
    n <- createElement e
    liftIO $ create c (DOM.castToElement n)
    return n
createElement (Tag s evs cs) = do
    d <- use document
    Just e <- liftIO $ DOM.documentCreateElement d s
    register e evs
    let n = DOM.toNode e
    updateChildren n (Delta [] cs)
    return n
createElement (Text s)       = do
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
