module Folds where

run :: IO ()
run = undefined
--
-- foldl :: (b -> a -> b) -> b -> t a -> b
-- foldl f zb [x, y, z] = ((zb `f` x) `f` y) `f` z
--
-- foldr :: (a -> b -> b) -> b -> t a -> b
-- foldr f zb [x, y, z] = x `f` (y `f` (z `f` zb))
--
-- foldq :: (b -> a -> b) -> b -> t a -> b
-- foldq f zb [x, y, z] = ((zb `f` z) `f` y) `f` x
--
--
--
--
--y = foldr (+) 0 [1, 2, 3]
--
---- (f 3 (f 2 (f 1 0)))
---- (3 `f` (2 `f` (1 `f` 0)))
--
--x = foldl (+) 0 [1, 2, 3]
--
---- (f (f (f 0 1) 2) 3)
---- (((0 `f` 1) `f` 2) `f` 3)
