module Lib where

import Data.List (elemIndices, find)

data GridParseError
  = GridEmpty
  | InconsistentLineLengths
  deriving (Show)

type Coord = (Int, Int)

data Grid = Grid
  { letters :: [Char]
  , width :: Int
  , height :: Int
  } deriving (Show)

getCoord :: Grid -> Int -> Coord
getCoord (Grid _ w _) index = (index `mod` w, index `div` w)

getIndex :: Grid -> Coord -> Int
getIndex (Grid _ w _) (x, y) = x + w * y

getCharOnIndex :: Grid -> Int -> Char
getCharOnIndex (Grid letters _ _) i = letters !! i

coordInBounds :: Grid -> Coord -> Bool
coordInBounds (Grid _ w h) (x, y) = x >= 0 && y >= 0 && x < w && y < h

getNthNeighbors :: Grid -> Int -> Int -> [Int]
getNthNeighbors grid i n' =
  let n = n' - 1
      nCoord (dx, dy) = (\(x, y) -> (x + dx, y + dy)) $ getCoord grid i
      deltas = tail [(x, y) | x <- [0, n, -n], y <- [0, n, -n]]
  in map (getIndex grid . nCoord) $ filter (coordInBounds grid . nCoord) deltas

getCoordsBetween :: Coord -> Coord -> [Coord]
getCoordsBetween (x1, y1) (x2, y2) = zip xs ys
  where
    (xs, ys)
      | abs (x2 - x1) == 0 = (repeat x1, [y1, y1 + signum (y2 - y1) .. y2])
      | abs (y2 - y1) == 0 = ([x1, x1 + signum (x2 - x1) .. x2], repeat y1)
      | otherwise = ([x1, x1 + signum (x2 - x1) .. x2], [y1, y1 + signum (y2 - y1) .. y2])


findWord :: Grid -> String -> (String, [Coord])
findWord grid@(Grid letters _ _) word =
  let outerCoords index = (getCoord grid <$> getNthNeighbors grid index (length word), getCoord grid index)
      coordsBetween = concatMap ((\(o, c) -> getCoordsBetween c <$> o) . outerCoords) $ elemIndices (head word) letters
      wordsFromCoords = ((\coord -> letters !! getIndex grid coord) <$>) <$> coordsBetween
  in case find (\(w, _) -> w == word) (zip wordsFromCoords coordsBetween) of
    Just (foundWord, foundCoords) -> (foundWord, foundCoords)
    Nothing -> ("", [(0, 0)])
