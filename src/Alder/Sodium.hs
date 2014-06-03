module Alder.Sodium
    ( Input(..)
    , newInput
    , filterInput
    , mount
    , select
    , (<@>)
    , (<@)
    ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Functor.Contravariant
import           Data.Monoid
import           FRP.Sodium

import           Alder.Html
import qualified Alder.Mount                as Alder

infixl 4 <@>, <@

newtype Input a = Input (a -> Reactive ())

instance Monoid (Input a) where
    mempty = Input $ \_ -> return ()
    mappend (Input f) (Input g) = Input $ \a -> f a >> g a

instance Contravariant Input where
    contramap f (Input g) = Input (g . f)

instance Handler Input where
    fire (Input f) = sync . f

newInput :: Reactive (Event a, Input a)
newInput = second Input <$> newEvent

filterInput :: (a -> Bool) -> Input a -> Input a
filterInput p (Input f) = Input $ \a -> if p a then f a else return ()

mount :: Behavior Html -> IO ()
mount b = do
    push <- Alder.mount
    void . sync $ listen (value b) push

select
    :: ((b -> Const (Endo [b]) b) -> a -> Const (Endo [b]) a)
    -> Event a
    -> Event b
select l = split . fmap toList
  where
    toList = flip appEndo [] . getConst . l (Const . Endo . (:))

(<@>) :: Behavior (a -> b) -> Event a -> Event b
b <@> e = snapshot (flip ($)) e b

(<@) :: Behavior a -> Event b -> Event a
b <@ e = snapshot (flip const) e b
