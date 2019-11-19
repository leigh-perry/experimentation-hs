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
  fmap a2b (Yoneda b2b12fb1) = Yoneda (\b2b1 -> b2b12fb1 (b2b1 . a2b))

--
data Coyoneda f a =
  forall x. Coyoneda (x -> a) (f x)

instance Functor (Coyoneda f) where
  fmap :: (a -> b) -> Coyoneda f a -> Coyoneda f b
  fmap a2b (Coyoneda x2a fx) = Coyoneda (a2b . x2a) fx
