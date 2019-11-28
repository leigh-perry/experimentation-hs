{-# LANGUAGE RankNTypes #-}

module ContT
  ( run
  ) where

import           Control.Monad.Identity (Identity, runIdentity)

run :: IO ()
run = do
  print $ calcK id
  print $ runIdentity calcT

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
