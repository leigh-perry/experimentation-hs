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
toYoneda fa = Yoneda (<$> fa)

fromYoneda :: Yoneda f a -> f a
fromYoneda (Yoneda ab1fb1) = ab1fb1 id

--
data Coyoneda f a =
  forall x. Coyoneda (x -> a) (f x)

instance Functor (Coyoneda f) where
  fmap :: (a -> b) -> Coyoneda f a -> Coyoneda f b
  fmap ab (Coyoneda xa fx) = Coyoneda (ab . xa) fx
