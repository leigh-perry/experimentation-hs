{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE RankNTypes                #-}

-- https://stackoverflow.com/a/24006085
module Yoneda
  ( run
  ) where

run :: IO ()
run = undefined -- TODO

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
  fmap f (Run scsmr) = Run $ fmap (fmap f) scsmr
  fmap f (Done a)    = Done (f a)

instance (Functor s, Functor m) => Functor (Coroutine s m) where
  fmap f cr = Coroutine (fmap (fmap f) (resume cr))

--
newtype Endo a =
  Endo
    { appEndo :: a -> a
    }

-- YEndo ~ Yoneda Endo
newtype YEndo a =
  YEndo
    { yEndo :: forall b1. (a -> b1) -> (b1 -> b1)
    }

instance Functor YEndo where
  fmap :: (a -> b) -> YEndo a -> YEndo b
  fmap f y = YEndo (\b2b1 -> yEndo y (b2b1 . f))

instance Applicative YEndo where
  pure :: a -> YEndo a
  pure a = YEndo (const id) -- TODO what about `a`?
  (<*>) :: YEndo (a -> b) -> YEndo a -> YEndo b
  (<*>) (YEndo a2b2e2e2e) (YEndo a2e2e2e) = undefined -- a2e2e2e  -- TODO

instance Monad YEndo where
  return :: a -> YEndo a
  return a = undefined  -- TODO
  (>>=) :: forall a b. YEndo a -> (a -> YEndo b) -> YEndo b
  (>>=) ma a2mb = undefined -- TODO
