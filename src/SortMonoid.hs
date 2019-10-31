module SortMonoid
  ( run
  ) where

run :: IO ()
run = print $ toSort ["string 3", "string 2", "string 1"]

toSort :: Ord a => [a] -> Sort a
toSort = foldMap (Sort . pure)

newtype Sort a =
  Sort [a]
  deriving (Show)

instance Ord a => Semigroup (Sort a) where
  (<>) (Sort x) (Sort y) = Sort $ mergeSort x y

instance Ord a => Monoid (Sort a) where
  mempty = Sort []

-- simple merge sort implementation
mergeSort :: Ord a => [a] -> [a] -> [a]
mergeSort [] xs = xs
mergeSort xs [] = xs
mergeSort (x:xs) (y:ys)
  | y < x = y : mergeSort (x : xs) ys
mergeSort (x:xs) ys = x : mergeSort xs ys
