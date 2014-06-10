{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Alder.Html.Internal
    ( -- * Elements
      Node(..)
      -- * Attributes
    , Attributes(..)
    , AttributeValue(..)
    , Handlers
      -- * Html
    , Html
    , HtmlM(..)
    , runHtml
    , parent
    , leaf
    , text
      -- * Event handlers
    , EventType(..)
    , Event(..)
    , Handler(..)
      -- * Setting attributes
    , Attribute
    , Attributable
    , (!)
    , (!?)
    , (!#)
    , (!.)
    , (!?.)
      -- * Creating attributes
    , token
    , tokenSet
    , boolean
    , onEvent
    ) where

import           Control.Applicative
import           Data.DList          as DList
import           Data.Hashable
import           Data.HashMap.Strict as HashMap hiding ((!))
import           Data.Monoid
import           Data.Text           as Text
import           Unsafe.Coerce

import           Alder.JavaScript

infixl 1 !, !?, !#, !., !?.

data Node
    = Element !Text Attributes [Node]
    | Text !Text

data Attributes = Attributes !(HashMap Text AttributeValue) !Handlers

data AttributeValue
    = Token !Text
    | TokenSet [Text]
    | Boolean
    deriving (Eq, Read, Show)

type Handlers = HashMap EventType (JSObj -> IO ())

type Html = HtmlM ()

newtype HtmlM a = HtmlM (Attributes -> DList Node)
    deriving (Monoid)

instance Functor HtmlM where
    fmap _ = unsafeCoerce

instance Applicative HtmlM where
    pure _ = mempty
    (<*>)  = appendHtml

instance Monad HtmlM where
    return _ = mempty
    (>>)     = appendHtml
    m >>= k  = m `appendHtml` k (error "Alder.HtmlM: monadic bind")

appendHtml :: HtmlM a -> HtmlM b -> HtmlM c
appendHtml a b = unsafeCoerce a <> unsafeCoerce b

runHtml :: Html -> [Node]
runHtml (HtmlM f) = DList.toList (f defaultAttributes)
  where
    defaultAttributes = Attributes HashMap.empty HashMap.empty

parent :: Text -> Html -> Html
parent t h = HtmlM $ \a -> DList.singleton (Element t a (runHtml h))

leaf :: Text -> Html
leaf t = HtmlM $ \a -> DList.singleton (Element t a [])

text :: Text -> Html
text t = HtmlM $ \_ -> DList.singleton (Text t)

addAttribute :: Attribute -> HtmlM a -> HtmlM a
addAttribute (Attribute f) (HtmlM g) = HtmlM (g . f)

data EventType
    = KeyDown | KeyPress | KeyUp
    | Focus | Blur
    | Input | Change
    | Submit
    | MouseDown | MouseUp | Click | DoubleClick | MouseMove | MouseEnter | MouseLeave
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance Hashable EventType where
    hashWithSalt s = hashWithSalt s . fromEnum

class Event e where
    extractEvent :: JSObj -> IO e

class Handler f where
    fire :: f e -> e -> IO ()

newtype Attribute = Attribute (Attributes -> Attributes)

instance Monoid Attribute where
    mempty = Attribute id
    mappend (Attribute f) (Attribute g) = Attribute (f . g)

class Attributable h where
    (!) :: h -> Attribute -> h

instance Attributable (HtmlM a) where
    (!) = flip addAttribute

instance Attributable h => Attributable (r -> h) where
    f ! a = (! a) . f

(!?) :: Attributable h => h -> (Bool, Attribute) -> h
h !? (p, a) = if p then h ! a else h

(!#) :: Attributable h => h -> Text -> h
h !# v = h ! token "id" v

(!.) :: Attributable h => h -> Text -> h
h !. v = h ! tokenSet "class" v

(!?.) :: Attributable h => h -> (Bool, Text) -> h
h !?. (p, v) = if p then h !. v else h

token :: Text -> Text -> Attribute
token k v = Attribute $ \(Attributes m hs) ->
    Attributes (HashMap.insert k (Token v) m) hs

tokenSet :: Text -> Text -> Attribute
tokenSet k v = Attribute $ \(Attributes m hs) ->
    let t = case HashMap.lookup k m of
            Just (TokenSet vs) -> TokenSet (v : vs)
            _                  -> TokenSet [v]
    in  Attributes (HashMap.insert k t m) hs

boolean :: Text -> Attribute
boolean k = Attribute $ \(Attributes m hs) ->
    Attributes (HashMap.insert k Boolean m) hs

onEvent :: (Handler f, Event e) => EventType -> f e -> Attribute
onEvent k handler = Attribute $ \(Attributes m hs) ->
    Attributes m (HashMap.insert k h hs)
  where
    h v = do
        e <- extractEvent v
        fire handler e
