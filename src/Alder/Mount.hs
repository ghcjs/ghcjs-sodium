
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE OverloadedStrings #-}
module Alder.Mount
    ( mount
    ) where

import           Control.Monad
import           Control.Monad.State.Class
import           Control.Monad.Trans
import           Data.Aeson
import           Data.HashMap.Strict       as HashMap
import           Data.IORef
import           Data.Maybe
import           Data.Monoid
import           Data.Text                 as Text
import           GHCJS.Foreign
import           GHCJS.Types

import           Alder.Html.Internal
import           Alder.IOState
import           Alder.JavaScript

data NativeNode

type DOMNode = JSRef NativeNode

type Name = Int

data MountState = MountState
    { nextName :: !Name
    , eventMap :: !(HashMap Name Element)
    }

type Mount = IOState MountState

ignore :: m () -> m ()
ignore = id

mount :: IO (Html -> IO ())
mount = do
    ref <- newIORef MountState
        { nextName = 0
        , eventMap = HashMap.empty
        }
    listen $ \target eventName obj ->
        runIOState (dispatch target eventName obj) ref
    return $ \html -> runIOState (update html) ref

listen :: (DOMElement -> Text -> Value -> IO ()) -> IO ()
listen callback = do
    events <- window .: "Events"
    jsCallback <- syncCallback1 AlwaysRetain False $ \params -> do
        target    <- readProp params "target"
        eventName <- readProp params "eventName"
        obj       <- readProp params "eventObject"
        callback target eventName obj
    ignore $ call events "listen" jsCallback

nameAttr :: Text
nameAttr = "data-alder-id"

register :: DOMElement -> Handlers -> Mount ()
register e h = do
    name <- gets nextName
    ignore $ apply e "setAttribute" (nameAttr, Text.pack $ show name)
    modify $ \s -> s
        { nextName = name + 1
        , eventMap = HashMap.insert name h (eventMap s)
        }

dispatch :: DOMElement -> Text -> Value -> Mount ()
dispatch target eventName obj = do
    field <- call target "getAttribute" nameAttr
    m <- gets eventMap
    fromMaybe (return ()) $ do
        name <- readMaybe (Text.unpack field)
        hs   <- HashMap.lookup name m
        h    <- HashMap.lookup eventName hs
        return $ liftIO (h obj)
  where
    readMaybe s = case reads s of
        [(a,"")] -> Just a
        _        -> Nothing

update :: Html -> Mount ()
update html = do
    let new = fromHtml html
    doc <- window .: "document"
    body <- readProp doc "body"
    modify $ \s -> s { eventMap = HashMap.empty }
    removeChildren body
    createChildren body new

create :: Element -> Mount DOMElement
create (Element t attrs cs) = do
    doc <- window .: "document"
    e <- call doc "createElement" t
    register e (handlers attrs)
    forM_ (HashMap.toList $ attributes attrs) $ \(k, v) ->
        ignore $ apply e "setAttribute" (k, v)
    createChildren e cs
    return e
create (Text t) = do
    doc <- window .: "document"
    call doc "createTextNode" t

createChildren :: DOMElement -> [Element] -> Mount ()
createChildren parent cs = do
    children <- mapM create cs
    forM_ children $ \child ->
        ignore $ call parent "appendChild" child

removeChildren :: DOMElement -> Mount ()
removeChildren parent = go
  where
    go = do
        r <- readProp parent "lastChild"
        case r of
            Nothing -> return ()
            Just c  -> do
                ignore $ call parent "removeChild" (c :: DOMElement)
                go
