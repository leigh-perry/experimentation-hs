{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE RankNTypes                #-}

-- https://stackoverflow.com/a/24006085
module Yoneda
  ( run
  ) where

run :: IO ()
run = undefined

newtype Yoneda f b =
  Yoneda
    { runYoneda :: forall b1. (b -> b1) -> f b1
    }

instance Functor (Yoneda f) where
  fmap :: (a -> b) -> Yoneda f a -> Yoneda f b
  fmap ab (Yoneda bb1fb1) = Yoneda (\bb1 -> bb1fb1 (bb1 . ab))

toYoneda :: Functor f => f a -> Yoneda f a
toYoneda fa = Yoneda (`fmap` fa)

fromYoneda :: Yoneda f a -> f a
fromYoneda (Yoneda ab1fb1) = ab1fb1 id

--
data Coyoneda f a =
  forall x. Coyoneda (x -> a) (f x)

instance Functor (Coyoneda f) where
  fmap :: (a -> b) -> Coyoneda f a -> Coyoneda f b
  fmap ab (Coyoneda xa fx) = Coyoneda (ab . xa) fx

--
toCoyoneda :: f a -> Coyoneda f a
toCoyoneda = Coyoneda id

fromCoyoneda :: Functor f => Coyoneda f a -> f a
fromCoyoneda (Coyoneda xa fx) = fmap xa fx

--
data Coroutine s m r =
  Coroutine
    { resume :: m (St s m r)
    }

data St s m r
  = Run (s (Coroutine s m r))
  | Done r

instance (Functor s, Functor m) => Functor (St s m) where
  fmap f (Run sCsmr) = Run $ fmap (fmap f) sCsmr
  fmap f (Done a)    = Done (f a)

instance (Functor s, Functor m) => Functor (Coroutine s m) where
  fmap f cr = Coroutine (fmap (fmap f) (resume cr))
