module Alder.Html.Internal
    ( -- * HTML
      Html
    , HtmlM(..)
      -- * Event handlers
    , Handlers
    , Handler(..)
      -- * Attributes
    , Attributes(..)
    , Attribute(..)
    , Attributable
    , (!)
    , (!?)
      -- * Setting attributes
    , attribute
    , boolean
    , onEvent
    ) where

import           Control.Applicative
import           Data.Aeson
import           Data.HashMap.Strict as HashMap hiding ((!))
import           Data.Monoid
import           Data.String
import           Data.Text           as Text
import           Unsafe.Coerce

infixl 1 !, !?

type Html = HtmlM ()

data HtmlM a
    = Empty
    | Append (HtmlM a) (HtmlM a)
    | Parent !Text (HtmlM a)
    | Leaf !Text
    | Content Text
    | AddAttribute Attribute (HtmlM a)

instance Monoid (HtmlM a) where
    mempty  = Empty
    mappend = Append

instance Functor HtmlM where
    fmap _ = unsafeCoerce

instance Applicative HtmlM where
    pure _ = Empty
    (<*>)  = appendHtml

instance Monad HtmlM where
    return _ = Empty
    (>>)     = appendHtml
    m >>= k  = m `appendHtml` k (error "Alder.HtmlM: monadic bind")

instance IsString (HtmlM a) where
    fromString = Content . fromString

appendHtml :: HtmlM a -> HtmlM b -> HtmlM c
appendHtml a b = unsafeCoerce a `Append` unsafeCoerce b

type Handlers = HashMap Text (Value -> IO ())

class Handler f where
    fire :: f e -> e -> IO ()

data Attributes = Attributes
    { attributes :: !(HashMap Text Text)
    , handlers   :: !Handlers
    }

instance Monoid Attributes where
    mempty = Attributes
        { attributes = mempty
        , handlers   = mempty
        }
    mappend a b = Attributes
        { attributes = merge attributes
        , handlers   = merge handlers
        }
      where
        merge f = f a <> f b

newtype Attribute = Attribute (Attributes -> Attributes)

instance Monoid Attribute where
    mempty = Attribute id
    mappend (Attribute f) (Attribute g) = Attribute (f . g)

class Attributable h where
    (!) :: h -> Attribute -> h

instance Attributable (HtmlM a) where
    h ! a = AddAttribute a h

instance Attributable h => Attributable (r -> h) where
    f ! a = (! a) . f

(!?) :: Attributable h => h -> (Bool, Attribute) -> h
h !? (p, a) = if p then h ! a else h

attribute :: Text -> Text -> Attribute
attribute k v = Attribute $ \a -> a { attributes = update (attributes a) }
  where
    update m = case HashMap.lookup k m of
        Nothing -> HashMap.insert k v m
        Just u  -> HashMap.insert k (Text.concat [u, Text.singleton ' ', v]) m

boolean :: Text -> Attribute
boolean k = attribute k Text.empty

onEvent :: (Handler f, FromJSON e) => Text -> f e -> Attribute
onEvent k handler = Attribute $ \a ->
    a { handlers = HashMap.insert k h (handlers a) }
  where
    h v = case fromJSON v of
        Error s   -> fail s
        Success e -> fire handler e

