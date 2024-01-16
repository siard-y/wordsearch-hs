module Lib where

data GridParseError
  = GridEmpty
  | InconsistentLineLengths
  deriving (Show)

data Grid = Grid
  { letters :: [Char]
  , width :: Int
  , height :: Int
  } deriving (Show)

gridFromFile :: FilePath -> IO (Either GridParseError Grid)
gridFromFile filePath = do
  fileContents <- readFile filePath
  let fileLines = lines fileContents
  return $ case fileLines of
    [] -> Left GridEmpty
    (firstLine : restLines) ->
      let width = length firstLine
          isValidLine l = length l == width
      in if all isValidLine restLines
           then Right $ Grid { letters = concat fileLines, width, height = length fileLines }
           else Left InconsistentLineLengths
