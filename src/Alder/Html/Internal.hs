{-# LANGUAGE ExistentialQuantification #-}
module Alder.Html.Internal
    ( -- * HTML
      Html
    , HtmlM(..)
      -- * Attributes
    , Attributes(..)
    , Attribute(..)
    , Attributable
    , (!)
    , (!?)
      -- * Event handlers
    , Handler(..)
    ) where

import           Control.Applicative
import           Data.Aeson
import           Data.HashMap.Strict as HashMap hiding ((!))
import           Data.HashSet        as HashSet
import           Data.Monoid
import           Data.String
import           Data.Text           as Text
import           Unsafe.Coerce

infixl 1 !, !?

type Html = HtmlM ()

data HtmlM a
    = Empty
    | forall b c. Append (HtmlM b) (HtmlM c)
    | Parent !Text (HtmlM a)
    | Leaf !Text
    | Content Text
    | AddAttribute Attribute (HtmlM a)

instance Functor HtmlM where
    fmap _ = unsafeCoerce

instance Applicative HtmlM where
    pure _ = Empty
    (<*>)  = Append

instance Monad HtmlM where
    return _ = Empty
    (>>)     = Append
    m >>= k  = m >> k (error "Alder.HtmlM: monadic bind")

instance IsString (HtmlM a) where
    fromString = Content . fromString


data Attributes = Attributes
    { classSet   :: !(HashSet Text)
    , attributes :: !(HashMap Text Text)
    , handlers   :: !(HashMap Text (Value -> IO ()))
    }

instance Monoid Attributes where
    mempty = Attributes
        { classSet   = mempty
        , attributes = mempty
        , handlers   = mempty
        }
    mappend a b = Attributes
        { classSet   = merge classSet
        , attributes = merge attributes
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

class Handler f where
    fire :: f e -> e -> IO ()
