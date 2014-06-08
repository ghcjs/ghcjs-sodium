{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Alder.Html.Internal
    ( -- * Elements
      Node(..)
    , Element(..)
      -- * Attributes
    , Attributes(..)
    , AttributeValue(..)
      -- * Html
    , Html
    , HtmlM(..)
    , runHtml
    , parent
    , leaf
    , text
      -- * Event handlers
    , Handler(..)
    , Event(..)
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
import           Data.HashMap.Strict as HashMap hiding ((!))
import           Data.Monoid
import           Data.Text           as Text
import           Unsafe.Coerce

import           Alder.JavaScript

infixl 1 !, !?, !#, !., !?.

data AttributeValue
    = Token !Text
    | TokenSet [Text]
    | Boolean
    deriving (Eq, Read, Show)

data Attributes = Attributes
    { attributeValues :: !(HashMap Text AttributeValue)
    , handlers        :: !(HashMap Text (JSObj -> IO ()))
    }

data Element = Element !Text Attributes

data Node
    = Parent Element [Node]
    | Text !Text

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

runHtml :: HtmlM a -> [Node]
runHtml (HtmlM f) = DList.toList (f defaultAttributes)
  where
    defaultAttributes = Attributes
        { attributeValues = HashMap.empty
        , handlers        = HashMap.empty
        }

appendHtml :: HtmlM a -> HtmlM b -> HtmlM c
appendHtml a b = unsafeCoerce a <> unsafeCoerce b

parent :: Text -> HtmlM a -> HtmlM a
parent t h = HtmlM $ \a -> DList.singleton (Parent (Element t a) (runHtml h))

leaf :: Text -> HtmlM a
leaf t = HtmlM $ \a -> DList.singleton (Parent (Element t a) [])

text :: Text -> HtmlM a
text t = HtmlM $ \_ -> DList.singleton (Text t)

addAttribute :: Attribute -> HtmlM a -> HtmlM a
addAttribute (Attribute f) (HtmlM g) = HtmlM (g . f)

class Handler f where
    fire :: f e -> e -> IO ()

class Event e where
    extractEvent :: JSObj -> IO e

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
h !. v = h ! tokenSet "className" v

(!?.) :: Attributable h => h -> (Bool, Text) -> h
h !?. (p, v) = if p then h !. v else h

attribute :: Text -> (Maybe AttributeValue -> AttributeValue) -> Attribute
attribute k f = Attribute $ \a ->
    a { attributeValues = update (attributeValues a) }
  where
    update m = HashMap.insert k (f (HashMap.lookup k m)) m

token :: Text -> Text -> Attribute
token k v = attribute k (\_ -> Token v)

tokenSet :: Text -> Text -> Attribute
tokenSet k v = attribute k $ \u -> case u of
    Just (TokenSet vs) -> TokenSet (v : vs)
    _                  -> TokenSet [v]

boolean :: Text -> Attribute
boolean k = attribute k (\_ -> Boolean)

onEvent :: (Handler f, Event e) => Text -> f e -> Attribute
onEvent k handler = Attribute $ \a ->
    a { handlers = HashMap.insert k h (handlers a) }
  where
    h v = do
        e <- extractEvent v
        fire handler e
