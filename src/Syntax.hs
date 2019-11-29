module Syntax
  ( run
  ) where

run :: IO ()
run = do
  print $ nestedMap (\i -> "Val: " <> show (i * 10)) (nestedMaybe 1)

nestedMaybe :: Int -> Maybe (Maybe Int)
nestedMaybe i = Just (Just i)

--nestedMap f mm = fmap (\mi -> fmap f mi) mm
--nestedMap f mm = fmap (fmap f) mm
--nestedMap f = fmap (fmap f)
--nestedMap f = (fmap . fmap) f
--nestedMap = fmap . fmap
nestedMap :: (Int -> String) -> Maybe (Maybe Int) -> Maybe (Maybe String)
nestedMap = fmap . fmap
