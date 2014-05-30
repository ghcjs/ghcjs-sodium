{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Alder.Mount
    ( Element(..)
    , mount
    ) where

import           Control.Applicative
import           Control.Lens              hiding (children)
import           Control.Monad
import           Control.Monad.State.Class
import           Control.Monad.Trans
import           Data.Aeson                (Value)
import           Data.HashMap.Strict       as HashMap
import           Data.IORef
import           Data.Text                 as Text
import           GHCJS.Types

import           Alder.IOState
import           Alder.JavaScript

data NativeElement

type DOMElement = JSRef NativeElement

data Element
    = Element !Text !(HashMap Text Text) (Value -> IO ()) [Element]
    | Text !Text

toHashMap :: Attributes -> HashMap Text Text
toHashMap attrs = Map.insert "class" classes (attributes attrs)
  where
    classes = Text.intercalate " " . HashSet.toList $ classSet attrs

handleEvent :: Attributes -> Text -> Value -> IO ()
handleEvent attrs eventName =
    Hashmap.lookup eventName (handlers attrs)

tag :: Text -> Attribute -> [Element] -> Element
tag t (Attribute f) = Tag t (toHashMap attrs) (handleEvent attrs)
  where
    attrs = f mempty

-- TODO: consolidate text nodes
fromHtml :: HtmlM a -> [Element]
fromHtml html = go mempty html []
  where
    go attr m = case m of
        Empty            -> id
        Append a b       -> go attr a . go attr b
        Parent t a       -> (tag t attr (fromHtml sa) :)
        Leaf t           -> (tag t attr [] :)
        Content t        -> (Text t :)
        AddAttribute a b -> go (a <> attr) b

type Name = Int

data MountState = MountState
    { _nextName :: !Name
    , _eventMap :: !(HashMap Name (Value -> IO ()))
    }

makeLenses ''MountState

newtype Mount a = Mount (IOState MountState a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadState MountState)

runMount :: Mount a -> IORef MountState -> IO a
runMount (Mount m) = runIOState m

ignore :: m () -> m ()
ignore = id

mount :: IO ([Element] -> IO ())
mount = do
    ref <- newIORef MountState
        { _nextName = 0
        , _eventMap = HashMap.empty
        }
    return $ \es -> runMount (update es) ref

nameAttr :: Text
nameAttr = "data-alder-id"

register :: DOMElement -> (Value -> IO ()) -> Mount ()
register e h = do
    name <- nextName <<+= 1
    ignore $ apply e "setAttribute" (nameAttr, Text.pack $ show name)
    eventMap . at name ?= h

dispatch :: DOMElement -> Value -> Mount ()
dispatch e ev = do
    name <- read . Text.unpack <$> call e "getAttribute" nameAttr
    r <- use (eventMap . at name)
    case r of
        Nothing -> return ()
        Just h  -> liftIO $ h ev

update :: [Element] -> Mount ()
update new = do
    doc <- global "document"
    body <- getProp doc "body"
    eventMap .= HashMap.empty
    removeChildren body
    createChildren body new

create :: Element -> Mount DOMElement
create (Element t attrs h cs) = do
    doc <- global "document"
    e <- call doc "createElement" t
    register e h
    forM_ (HashMap.toList attrs) $ \(k, v) ->
        ignore $ apply e "setAttribute" (k, v)
    createChildren e cs
    return e
create (Text t) = do
    doc <- global "document"
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
        r <- getProp parent "lastChild"
        case r of
            Nothing -> return ()
            Just c  -> do
                ignore $ call parent "removeChild" (c :: DOMElement)
                go
