module Main where

import qualified Lib (someFunc)

main :: IO ()
main = do
  putStrLn "Hallo liebe Leute!"
  Lib.someFunc
