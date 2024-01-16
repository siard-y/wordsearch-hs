module Main where

import Options.Applicative
import Lib

data FileInput = FileInput
  { gridFile    :: FilePath
  , wordsFile   :: FilePath }

fileInput :: Parser FileInput
fileInput = FileInput
      <$> strOption
          (  long "grid"
          <> short 'g'
          <> metavar "GRID_FILE"
          <> help "Word search grid file" )
      <*> strOption
          (  long "words"
          <> short 'w'
          <> metavar "WORDS_FILE"
          <> help "Word search word list file" )

run :: FileInput -> IO ()
run options = putStrLn "placeholder"

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (fileInput <**> helper)
      ( fullDesc
     <> progDesc "Find words in a word search puzzle"
     <> header "Word search puzzle solver" )
     