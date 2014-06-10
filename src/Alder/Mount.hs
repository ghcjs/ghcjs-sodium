
{-# LANGUAGE OverloadedStrings #-}
module Alder.Mount
    ( mount
    ) where

import           Control.Monad
import           Control.Monad.State.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.HashMap.Strict       as HashMap
import           Data.IORef
import           Data.Text                 as Text hiding (index)
import           GHCJS.Foreign

import           Alder.Diff
import           Alder.Html.Internal
import           Alder.IOState
import           Alder.JavaScript

type DOMNode = JSObj

type Name = Int

data MountState = MountState
    { nextName :: !Name
    , model    :: [Node]
    , handlers :: !(HashMap Name Handlers)
    }

type Mount = IOState MountState

ignore :: m () -> m ()
ignore = id

readMaybe :: (MonadPlus m, Read a) => String -> m a
readMaybe s = case reads s of
    [(a,"")] -> return a
    _        -> mzero

mount :: IO (Html -> IO ())
mount = do
    ref <- newIORef MountState
        { nextName = 0
        , model    = []
        , handlers = HashMap.empty
        }
    listen $ \eventType ev -> runIOState (dispatch eventType ev) ref
    document <- window .: "document"
    body <- document .: "body"
    removeChildren body
    return $ \html -> runIOState (update html) ref

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

register :: DOMNode -> Handlers -> Mount ()
register n hs = do
    name <- gets nextName
    ignore $ apply n "setAttribute" (nameAttr, Text.pack $ show name)
    modify $ \s -> s
        { nextName = name + 1
        , handlers = HashMap.insert name hs (handlers s)
        }

dispatch :: EventType -> JSObj -> Mount ()
dispatch eventType ev = void . runMaybeT $ do
    target <- MaybeT $ ev .: "target"
    attr   <- MaybeT $ call target "getAttribute" nameAttr
    name   <- readMaybe (Text.unpack attr)
    hs     <- MaybeT $ gets (HashMap.lookup name . handlers)
    case HashMap.lookup eventType hs of
        Nothing -> return ()
        Just h  -> liftIO $ h ev

update :: Html -> Mount ()
update html = do
    old <- gets model
    let new = runHtml html
    document <- window .: "document"
    body <- document .: "body"
    modify $ \s -> s { model = new, handlers = HashMap.empty }
    index <- liftIO $ createIndex body
    updateChildren index body (diff old new)

attributeValue :: AttributeValue -> Text
attributeValue (Token t)     = t
attributeValue (TokenSet ts) = Text.intercalate " " (Prelude.reverse ts)
attributeValue Boolean       = ""

updateChildren :: HashMap Id DOMNode -> DOMNode -> Diff -> Mount ()
updateChildren index n d0 = do
    c <- n .: "firstChild"
    go c d0
  where
    go c' (Match i a d1 d2) = do
        let c = case HashMap.lookup i index of
                Nothing -> error $ "updateChildren: id " ++
                                   show i ++ " not found"
                Just r  -> r

        r <- c .: "parentNode"
        case r of
            Nothing -> return ()
            Just p  -> call p "removeChild" c

        updateAttributes c a
        updateChildren index c d1
        ignore $ apply n "insertBefore" (c, c')
        go c' d2

    go (Just c) (Relabel a d1 d2) = do
        c' <- c .: "nextSibling"
        updateAttributes c a
        updateChildren index c d1
        go c' d2

    go (Just c) (Revalue t d) = do
        c' <- c .: "nextSibling"
        writeProp c "nodeValue" t
        go c' d

    go c' (Add node d) = do
        c <- create node
        ignore $ apply n "insertBefore" (c, c')
        go c' d

    go (Just c) (Drop d) = do
        c' <- c .: "nextSibling"
        ignore $ call n "removeChild" c
        go c' d

    go (Just c) (Pass d) = do
        c' <- c .: "nextSibling"
        go c' d

    go Nothing End = return ()

    go _ d = error $ "updateChildren: invalid diff " ++ show d

updateAttributes :: DOMNode -> AttributesDiff -> Mount ()
updateAttributes n (AttributesDiff old new hs) = do
    register n hs
    forM_ old $ \k ->
        ignore $ call n "removeAttribute" k
    forM_ new $ \(k, v) ->
        ignore $ apply n "setAttribute" (k, attributeValue v)

create :: Node -> Mount DOMNode
create (Element t (Attributes m hs) cs) = do
    document <- window .: "document"
    n <- call document "createElement" t
    register n hs
    forM_ (HashMap.toList m) $ \(k, v) ->
        ignore $ apply n "setAttribute" (k, attributeValue v)
    createChildren n cs
    return n
create (Text t) = do
    document <- window .: "document"
    call document "createTextNode" t

createChildren :: DOMNode -> [Node] -> Mount ()
createChildren n cs = do
    children <- mapM create cs
    forM_ children $ \child ->
        ignore $ call n "appendChild" child

createIndex :: DOMNode -> IO (HashMap Id DOMNode)
createIndex n = do
    list <- call n "getElementsByTagName" ("*" :: Text)
    len  <- list .: "length"
    go HashMap.empty (len - 1 :: Int) list
  where
    go m j list
        | j < 0     = return m
        | otherwise = do
            e <- call list "item" j
            attr <- call e "getAttribute" ("id" :: Text)
            case attr of
                Nothing -> go m (j - 1) list
                Just i  -> go (HashMap.insert i e m) (j - 1) list

removeChildren :: DOMNode -> IO ()
removeChildren n = go
  where
    go = do
        r <- n .: "lastChild"
        case r of
            Nothing -> return ()
            Just c  -> do
                ignore $ call n "removeChild" (c :: DOMNode)
                go
