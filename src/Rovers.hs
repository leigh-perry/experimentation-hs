module Rovers
  ( run
  ) where

import           Data.Bifunctor (first)
import           Data.Bool
import           Data.Vector    ((!))
import qualified Data.Vector    as V

{-
  Plateau
    { pTopRight = Coord {cx = 5, cy = 5}
    , pFinished =
      [ Rover {rCoord = Coord {cx = 1, cy = 3}, rDirection = North}
      , Rover {rCoord = Coord {cx = 5, cy = 1}, rDirection = East}
      ]
    }
-}
run :: IO ()
run = do
  text <- readFile "data/rovers.input"
  let (roverInstructions, plateau) = decodeConfig text
  let outcome = driveRovers roverInstructions plateau
  print outcome

----
data Direction
  = North
  | East
  | South
  | West
  deriving (Show, Eq)

data Coord =
  Coord
    { cx :: Int
    , cy :: Int
    }
  deriving (Show, Eq)

data Rover =
  Rover
    { rCoord     :: Coord
    , rDirection :: Direction
    }
  deriving (Show)

data Plateau =
  Plateau
    { pTopRight :: Coord
    , pFinished :: [Rover]
    }
  deriving (Show)

---- solution logic
driveRovers :: [(Rover, String)] -> Plateau -> Plateau
driveRovers roverMoves plateau = foldl f plateau roverMoves
  where
    f p (r, i) = p {pFinished = pFinished p ++ [driveRover i p r]}

driveRover :: String -> Plateau -> Rover -> Rover
driveRover moves plateau rover = foldl (\r i -> updateRover i plateau r) rover moves

updateRover :: Char -> Plateau -> Rover -> Rover
updateRover instruction plateau (Rover c d) = Rover c' d'
  where
    (c', d') =
      case instruction of
        'L' -> (c, rotate 3 d)
        'R' -> (c, rotate 1 d)
        'M' -> (moveOne d plateau c, d)
        _   -> error $ "Illegal instruction " <> show instruction

rotate :: Int -> Direction -> Direction
rotate delta d = lookup ! i
  where
    lookup = V.fromList [North, East, South, West]
    currentIndex =
      case V.findIndex (d ==) lookup of
        (Just ii) -> ii
        _         -> error ("Impossible direction " <> show d)
    i = (currentIndex + delta) `mod` length lookup

moveOne :: Direction -> Plateau -> Coord -> Coord
moveOne d plateau c = bool c' c (isOccupied plateau c') -- bail on occupied cells
  where
    x = cx c
    y = cy c
    topRight = pTopRight plateau
    c' =
      case d of
        North -> bool c (Coord x (y + 1)) (y < cy topRight)
        South -> bool c (Coord x (y - 1)) (y > 0)
        East  -> bool c (Coord (x + 1) y) (x < cx topRight)
        West  -> bool c (Coord (x - 1) y) (x > 0)

isOccupied :: Plateau -> Coord -> Bool
isOccupied p c = c `elem` (rCoord <$> pFinished p)

---- crufy code for reading the config
decodeConfig :: String -> ([(Rover, String)], Plateau)
decodeConfig text = (roverInstructions, Plateau topRight [])
  where
    ls = lines text
    topRight = decodeCoord $ head $ take 1 ls
    roverLinePairs = splitEvery 2 $ drop 1 ls -- [[String]] eg [["3 3 E", "MMRMMRMRRM"], ...]
    roverLineTuples = toTuple2 <$> roverLinePairs -- [("3 3 E", "MMRMMRMRRM"), ...]
    roverInstructions = first lineToRover <$> roverLineTuples

decodeCoord :: String -> Coord
decodeCoord line = toCoord $ decodeInt <$> words line
  where
    toCoord l = uncurry Coord $ toTuple2 l

lineToRover :: String -> Rover -- eg 3 3 E
lineToRover s = toRover $ toTuple3 $ words s
  where
    toRover (x, y, d) = Rover (Coord (decodeInt x) (decodeInt y)) (decodeDirection d)

decodeInt :: String -> Int
decodeInt s = read s :: Int

decodeDirection :: String -> Direction
decodeDirection "N" = North
decodeDirection "E" = East
decodeDirection "S" = South
decodeDirection "W" = West
decodeDirection s   = error $ "Unsupported direction " <> show s

---- utils
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs
  where
    (as, bs) = splitAt n xs

toTuple2 :: Show a => [a] -> (a, a)
toTuple2 [x, y] = (x, y)
toTuple2 ss     = error $ "Error decoding tuple2 " <> show ss

toTuple3 :: Show a => [a] -> (a, a, a)
toTuple3 [x, y, z] = (x, y, z)
toTuple3 ss        = error $ "Error decoding tuple3 " <> show ss
