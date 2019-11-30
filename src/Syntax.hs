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

----
data Product

data Html =
  Html
  deriving (Eq)

getProduct :: tring -> Maybe Product
getProduct = undefined

renderPage :: Product -> Html
renderPage = undefined

compose1 :: Bool
compose1 = bracketStyle == composeStyle
  where
    bracketStyle = fmap renderPage (getProduct "asdf")
    composeStyle = (fmap renderPage . getProduct) "asdf"

----
