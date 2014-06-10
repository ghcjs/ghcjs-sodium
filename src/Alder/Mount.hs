
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
import           Data.Text                 as Text
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
    , nameMap  :: !(HashMap Name Node)
    }

type Mount = IOState MountState

ignore :: m () -> m ()
ignore = id

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(a,"")] -> Just a
    _        -> Nothing

mount :: IO (Html -> IO ())
mount = do
    ref <- newIORef MountState
        { nextName = 0
        , model    = []
        , nameMap  = HashMap.empty
        }
    listen $ \eventType ev -> runIOState (dispatch eventType ev) ref
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

register :: DOMNode -> Node -> Mount ()
register n e = do
    name <- gets nextName
    ignore $ apply n "setAttribute" (nameAttr, Text.pack $ show name)
    modify $ \s -> s
        { nextName = name + 1
        , nameMap  = HashMap.insert name e (nameMap s)
        }

retrieve :: DOMNode -> Mount (Maybe Node)
retrieve n = runMaybeT $ do
    attr <- MaybeT $ call n "getAttribute" nameAttr
    name <- MaybeT . return $ readMaybe (Text.unpack attr)
    MaybeT $ gets (HashMap.lookup name . nameMap)

dispatch :: EventType -> JSObj -> Mount ()
dispatch eventType ev = void . runMaybeT $ do
    target  <- MaybeT $ ev .: "target"
    Element _ attrs _ <- MaybeT $ retrieve target
    case HashMap.lookup eventType (handlers attrs) of
        Nothing -> return ()
        Just h  -> liftIO $ h ev

update :: Html -> Mount ()
update html = do
    old <- gets model
    let new = runHtml html
    liftIO . print $ diffForests old new
    document <- window .: "document"
    body <- document .: "body"
    removeChildren body
    modify $ \s -> s { model = new, nameMap = HashMap.empty }
    createChildren body new

attributeValue :: AttributeValue -> Text
attributeValue (Token t)     = t
attributeValue (TokenSet ts) = Text.intercalate " " (Prelude.reverse ts)
attributeValue Boolean       = ""

create :: Node -> Mount DOMNode
create e@(Element t attrs cs) = do
    document <- window .: "document"
    n <- call document "createElement" t
    register n e
    forM_ (HashMap.toList $ attributeValues attrs) $ \(k, v) ->
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

removeChildren :: DOMNode -> Mount ()
removeChildren n = go
  where
    go = do
        r <- n .: "lastChild"
        case r of
            Nothing -> return ()
            Just c  -> do
                ignore $ call n "removeChild" (c :: DOMNode)
                go
