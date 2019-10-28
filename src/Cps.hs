{-# LANGUAGE InstanceSigs #-}

module Cps
  ( run
  ) where

run :: IO ()
run = do
  print $ eg1 3
  print $ eg2 3
  print $ eg3 3
  print $ eg4composition 3
  print $ eg5composition 3
  print $ eg6 3

eg1 :: Int -> Bool
eg1 i = 10 < times3 i
  where
    times3 :: Int -> Int
    times3 n = n * 3

eg2 :: Int -> Bool
eg2 i = cps i (\j -> 10 < j)
  where
    cps :: Int -> (Int -> Bool) -> Bool
    cps n p = p (n * 3)

eg3 :: Int -> Bool
eg3 i = cps (* 3) i (10 <)
  where
    cps :: (a -> b) -> a -> (b -> c) -> c
    cps f a p = p (f a)

eg4composition :: Int -> Bool
eg4composition x = times3 x (\y -> greaterThan10 y (\z -> z)) -- same shape as flatMap flatMap map
  where
    times3 :: Int -> (Int -> r) -> r
    times3 n c = c (n * 3)
    greaterThan10 :: Int -> (Bool -> r) -> r
    greaterThan10 i c = c (10 < i)

-- point free
eg5composition :: Int -> Bool
eg5composition = times3 (greaterThan10 id)
  where
    times3 :: (Int -> r) -> Int -> r
    times3 c n = c (n * 3)
    greaterThan10 :: (Bool -> r) -> Int -> r
    greaterThan10 c i = c (10 < i)

newtype Cont r a =
  Cont ((a -> r) -> r)

runCont :: Cont r a -> (a -> r) -> r
runCont (Cont ff) = ff

evalCont :: Cont a a -> a
evalCont (Cont ff) = ff id

pureCont :: a -> Cont r a
pureCont a = Cont (\c -> c a)

-- TODO follow types
bindCont :: Cont r a -> (a -> Cont r b) -> Cont r b
bindCont (Cont ma) mc_ = Cont (\k -> ma (\a -> mc a (\b -> k b)))
  where
    mc = runCont . mc_

instance Functor (Cont r) where
  fmap :: (a -> b) -> Cont r a -> Cont r b
  fmap f (Cont m) = Cont (\k -> m (k . f))

instance Applicative (Cont r) where
  pure :: a -> Cont r a
  pure = pureCont
  (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b
  Cont mf <*> Cont ma = Cont (\k -> mf (\f -> ma (\a -> k (f a))))

instance Monad (Cont r) where
  (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
  (>>=) = bindCont

eg6 :: Int -> Bool
eg6 x =
  evalCont $ do
    y <- timesThree x
    greaterThanTen y
  where
    timesThree :: Int -> Cont r Int
    timesThree i = Cont (\k -> k (3 * i))
    greaterThanTen :: Int -> Cont r Bool
    greaterThanTen y = Cont (\k -> k (10 < y))
