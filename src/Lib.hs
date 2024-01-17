module Lib where

import Data.List (elemIndices)

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
getCoord grid index = (index `mod` width grid, index `div` width grid)

getIndex :: Grid -> Coord -> Int
getIndex grid (x, y) = x + width grid * y

getCharOnIndex :: Grid -> Int -> Char
getCharOnIndex grid i = letters grid !! i

coordInBounds :: Grid -> Coord -> Bool
coordInBounds grid (x, y) = x >= 0 && y >= 0 && x < width grid && y < height grid

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
  let filtered = elemIndices (head word) letters
      outerLettersIndices = [((x, letters !! x), getNthNeighbors grid x (length word)) | x <- filtered]
      -- outerLetters = map (second (map (letters !!))) outerLettersIndices
      outerLetters = map (\(i, o) -> (i, (o, map (letters !!) o))) outerLettersIndices
      -- lll = map (second (filter (== last word))) outerLetters
      lll = map (\(i, (oi, oc)) -> (i, (oi, filter (== last word) oc))) outerLetters
      nnnn = map (\((ii, ij), (oi, oc)) -> ((getCoord grid ii, ij), (map (getCoord grid) oi, oc))) lll
      coordsBetween = map (\((icrd, ichr), (oi, ocs)) -> ((icrd, ichr), (map (getCoordsBetween icrd) oi, ocs))) nnnn
  in [ HELP AAAAAAA | ((a, b), (c, d)) <- coordsBetween]