{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE RankNTypes       #-}

module ContT
  ( run
  ) where

import           Control.Monad.Identity (Identity, runIdentity)

-- https://maxhallinan.com/posts/2019/10/22/how-does-the-continuation-monad-work/
run :: IO ()
run = do
  print $ calcK id
  print $ runIdentity calcT
  print $ runContT result1 pure
  print $ runContT result2 pure

-- `((Int -> r) -> r)` is a continuation that returns an `r`
cadd :: forall r. Int -> Int -> ((Int -> r) -> r)
cadd x y k = k (x + y)

cmult :: forall r. Int -> Int -> ((Int -> r) -> r)
cmult x y k = k (x * y)

csquare :: forall r. Int -> Int -> ((Int -> r) -> r)
csquare x y k = k (x * y)

-- 3 * (2 ^ 2) + 1
calcK :: (Int -> r) -> r
calcK k = csquare 2 2 (\x -> cmult 3 x (\y -> cadd 1 y k))

-- `((Int -> r) -> r)` is a continuation that returns an `r`
-- Generalise to `r` to a type-constructed `m r`
newtype ContT r m a =
  ContT ((a -> m r) -> m r)

ctadd :: forall r m. Int -> Int -> ContT r m Int
ctadd x y = ContT (\k -> k (x + y))

ctmult :: forall r m. Int -> Int -> ContT r m Int
ctmult x y = ContT (\k -> k (x * y))

ctsquare :: forall r m. Int -> Int -> ContT r m Int
ctsquare x y = ContT (\k -> k (x * y))

runContT :: forall r m a. ContT r m a -> (a -> m r) -> m r
runContT (ContT ck) k = ck k

calcT :: Identity Int
calcT = runContT (ctsquare 2 2) (\x -> runContT (ctmult 3 x) (\y -> runContT (ctadd 1 y) pure))

bindContT :: forall r m a b. ContT r m a -> (a -> ContT r m b) -> ContT r m b
bindContT (ContT m) -- m :: (a -> m r) -> m r
           f -- f :: a -> ContT r m b
 = ContT (\k -> m (\a -> runContT (f a) k))

instance Functor (ContT r m) where
  fmap :: (a -> b) -> ContT r m a -> ContT r m b
  fmap f (ContT m) = ContT (\k -> m (k . f))

instance Applicative m => Applicative (ContT r m) where
  pure :: a -> ContT r m a
  pure a = ContT (\a2mr -> a2mr a)
  (<*>) :: ContT r m (a -> b) -> ContT r m a -> ContT r m b
  ContT mf <*> ContT ma = ContT (\k -> mf (\f -> ma (k . f)))

instance Monad m => Monad (ContT r m) where
  (>>=) :: ContT r m a -> (a -> ContT r m b) -> ContT r m b
  (>>=) = bindContT

result1 :: ContT Int Identity Int
result1 = ctsquare 2 2 >>= ctmult 3 >>= ctadd 1

result2 :: ContT Int Identity Int
result2 = do
  step1 <- ctsquare 2 2
  step2 <- ctmult 3 step1
  ctadd 1 step2
