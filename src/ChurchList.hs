{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE RankNTypes                #-}

module ChurchList
  ( run
  , nil2
  , cons2
  , map2
  , displayList2
  ) where

run :: IO ()
run = undefined -- TODO

-- from Tony Morris
newtype List2 a =
  List2 (forall x. (a -> x -> x) -> x -> x)

nil2 :: List2 a
nil2 = List2 (const id)

cons2 :: a -> List2 a -> List2 a
--invoke the lower foldright and then put the a into it
cons2 a (List2 axxxx) = List2 (\axx -> axx a . axxxx axx)

map2 :: (a -> b) -> List2 a -> List2 b
map2 f (List2 axxxx) = List2 (\bxx -> axxxx (bxx . f))

displayList2 :: List2 a -> [a]
displayList2 (List2 axxxx) = axxxx (:) []
-- > displayList2 (cons2 "a" nil2)
-- ["a"]
-- > displayList2 (cons2 "a" (cons2 "b" nil2))
-- ["a","b"]
-- > displayList2 (map2 (toUpper <$>) (cons2 "a" (cons2 "b" nil2)))
-- ["A","B"]
