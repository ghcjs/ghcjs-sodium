module Alder.Sodium
    ( Input(..)
    , newInput
    , mount
    , keep
    , pass
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

mount :: Behavior Html -> IO ()
mount b = do
    push <- Alder.mount
    void . sync $ listen (value b) push

type Getting r s a = (a -> Const r a) -> s -> Const r s

toListOf :: Getting (Endo [b]) a b -> a -> [b]
toListOf l = flip appEndo [] . getConst . l (Const . Endo . (:))

keep :: (a -> Bool) -> Input a -> Input a
keep p (Input f) = Input $ \a -> when (p a) $ f a

pass :: Getting (Endo [b]) a b -> Input b -> Input a
pass l (Input f) = Input $ \a -> mapM_ f (toListOf l a)

select :: Getting (Endo [b]) a b -> Event a -> Event b
select l = split . fmap (toListOf l)

(<@>) :: Behavior (a -> b) -> Event a -> Event b
b <@> e = snapshot (flip ($)) e b

(<@) :: Behavior a -> Event b -> Event a
b <@ e = snapshot (flip const) e b
