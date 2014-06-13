
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Alder.Mount
    ( mount
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import qualified Data.Foldable             as Foldable
import           Data.HashMap.Strict       as HashMap
import           Data.IORef
import           Data.Maybe
import           Data.Text                 as Text
import           Data.Tree
import           GHCJS.Foreign

import           Alder.Reconcile
import           Alder.Html.Internal
import           Alder.IOState
import           Alder.JavaScript
import           Alder.Unique

type DOMNode = JSObj

data NodeInfo = NodeInfo !Unique !Attributes DOMNode

data MountState = MountState
    { nextUnique   :: !Unique
    , model        :: TForest Node
    , prevElements :: !(HashMap Unique NodeInfo)
    , elements     :: !(HashMap Unique NodeInfo)
    }

newtype Mount a = Mount (IOState MountState a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadState MountState)

instance MonadSupply Mount where
    getUnique = state $ \s ->
        let i = nextUnique s
        in  (i, s { nextUnique = i + 1 })

runMount :: Mount a -> IORef MountState -> IO a
runMount (Mount m) = runIOState m

ignore :: m () -> m ()
ignore = id

pointwiseM :: Monad m => (a -> b -> m Bool) -> [a] -> [b] -> m Bool
pointwiseM p = go
  where
    go []     []     = return True
    go []     _      = return False
    go _      []     = return False
    go (x:xs) (y:ys) = do r <- p x y
                          if r then go xs ys else return False

optionally :: Functor m => MaybeT m a -> m ()
optionally = void . runMaybeT

readMaybe :: (MonadPlus m, Read a) => String -> m a
readMaybe s = case reads s of
    [(a,"")] -> return a
    _        -> mzero

liftMaybe :: MonadPlus m => Maybe a -> m a
liftMaybe = maybe mzero return

mount :: IO (Html -> IO ())
mount = do
    ref <- newIORef MountState
        { nextUnique   = 0
        , model        = []
        , prevElements = HashMap.empty
        , elements     = HashMap.empty
        }
    listen $ \eventType ev -> runMount (dispatch eventType ev) ref
    return $ \html -> runMount (update html) ref

listen :: (EventType -> JSObj -> IO ()) -> IO ()
listen callback = do
    document <- window .: "document"
    forM_ [minBound .. maxBound] $ \eventType -> do
        jsCallback <- syncCallback1 AlwaysRetain False $ callback eventType
        ignore $ apply document "addEventListener"
            (eventName eventType, jsCallback, useCapture eventType)

eventName :: EventType -> Text
eventName eventType = case eventType of
    DoubleClick -> "dblclick"
    MouseEnter  -> "mouseover"
    MouseLeave  -> "mouseout"
    _           -> Text.toLower . Text.pack $ show eventType

useCapture :: EventType -> Bool
useCapture = (`elem` [Focus, Blur, Submit])

nameAttr :: Text
nameAttr = "data-alder-id"

getId :: DOMNode -> Mount (Maybe Unique)
getId n = runMaybeT $ do
    attr <- MaybeT $ call n "getAttribute" nameAttr
    readMaybe (Text.unpack attr)

register :: NodeInfo -> Mount ()
register info@(NodeInfo i _ n) = do
    ignore $ apply n "setAttribute" (nameAttr, Text.pack $ show i)
    modify $ \s -> s { elements = HashMap.insert i info (elements s) }

dispatch :: EventType -> JSObj -> Mount ()
dispatch eventType ev = optionally $ do
    target <- MaybeT $ ev .: "target"
    i <- MaybeT $ getId target
    NodeInfo _ attrs _ <- MaybeT $ gets (HashMap.lookup i . elements)
    h <- liftMaybe $ HashMap.lookup eventType (handlers attrs)
    liftIO $ h ev
    when (eventType `elem` [Input, Change]) $ liftIO $ syncValue target

update :: Html -> Mount ()
update html = do
    old <- gets model
    new <- reconcile (runHtml html) old
    document <- window .: "document"
    body     <- document .: "body"
    modify $ \s -> s
        { model        = new
        , prevElements = elements s
        , elements     = HashMap.empty
        }
    updateChildren body new

updateChildren :: DOMNode -> TForest Node -> Mount ()
updateChildren n xs = do
    cs <- getChildren n
    r <- pointwiseM matches cs xs
    if r then
            mapM_ (uncurry applyNode) (Prelude.zip cs xs)
        else do
            cs' <- mapM fetchNode xs
            removeChildren n
            appendChildren n cs'
  where
    matches c (Node (i :< Element{}) _) = do
        nodeType <- c .: "nodeType"
        case nodeType :: Int of
            1 -> (== Just i) <$> getId c
            _ -> return False

    matches c (Node (_ :< Text{}) _) =
        (== (3 :: Int)) <$> c .: "nodeType"

fetchNode :: TTree Node -> Mount DOMNode
fetchNode t@(Node (i :< Element{}) _) = do
    m <- gets prevElements
    case HashMap.lookup i m of
        Nothing               -> create t
        Just (NodeInfo _ _ c) -> do
            p <- c .: "parentNode"
            Foldable.mapM_ (\n -> ignore $ call n "removeChild" c)
                (p :: Maybe DOMNode)
            applyNode c t
            return c
fetchNode t = create t

applyNode :: DOMNode -> TTree Node -> Mount ()
applyNode n (Node (i :< Element _ a2) cs) = do
    NodeInfo _ a1 _ <- fromMaybe
        (error $ "applyNode: missing node " ++ show i) <$>
        gets (HashMap.lookup i . prevElements)
    setAttributes i n a1 a2
    updateChildren n cs
applyNode n (Node (_ :< Text t) _) =
    writeProp n "nodeValue" t

setAttributes :: Unique -> DOMNode -> Attributes -> Attributes -> Mount ()
setAttributes i n a1 a2 = do
    register (NodeInfo i a2 n)
    writeProp n "id" (idName a2)
    writeProp n "className" (className a2)
    forM_ old $ \k ->
        ignore $ call n "removeAttribute" k
    forM_ new $ \(k, v) ->
        ignore $ apply n "setAttribute" (k, v)
    liftIO $ syncValue n
  where
    idName    = fromMaybe "" . elementId
    className = Text.intercalate " " . Prelude.reverse . elementClass

    (m1, m2) = (otherAttributes a1, otherAttributes a2)

    old = Prelude.filter dead (HashMap.keys m1)
    new = Prelude.filter replaces (HashMap.toList m2)

    dead k = not (HashMap.member k m2)

    replaces (k, v) = case HashMap.lookup k m1 of
        Just u | u == v -> False
        _               -> True

create :: TTree Node -> Mount DOMNode
create (Node (i :< Element t a) cs) = do
    document <- window .: "document"
    n <- call document "createElement" t
    setAttributes i n defaultAttributes a
    children <- mapM create cs
    appendChildren n children
    return n
create (Node (_ :< Text t) _) = do
    document <- window .: "document"
    call document "createTextNode" t

syncValue :: DOMNode -> IO ()
syncValue n = optionally $ do
    tagName <- n .: "tagName"
    guard $ (tagName :: Text) `elem` ["INPUT", "TEXTAREA", "SELECT"]

    inputType <- n .: "type"
    when ((inputType :: Text) `elem` ["checkbox", "radio"]) $ do
        checked <- call n "hasAttribute" ("checked" :: Text)
        writeProp n "checked" (checked :: Bool)

    attr <- call n "getAttribute" ("value" :: Text)
    val  <- n .: "value"
    when (attr /= val) $ writeProp n "value" (attr :: Text)

getChildren :: MonadIO m => DOMNode -> m [DOMNode]
getChildren n = do
    c <- n .: "firstChild"
    go c
  where
    go Nothing  = return []
    go (Just c) = do
        c' <- c .: "nextSibling"
        (c :) `liftM` go c'

appendChildren :: MonadIO m => DOMNode -> [DOMNode] -> m ()
appendChildren n = mapM_ (ignore . call n "appendChild")

removeChildren :: (Functor m, MonadIO m) => DOMNode -> m ()
removeChildren n = optionally . forever $ do
    c <- MaybeT $ n .: "lastChild"
    ignore $ call n "removeChild" (c :: DOMNode)
