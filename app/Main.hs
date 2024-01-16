module Main where

import Options.Applicative
import Lib

main :: IO ()
main = do
  putStrLn "Hallo liebe Leute!"
  Lib.someFunc
