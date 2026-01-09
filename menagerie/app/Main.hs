module Main where

import qualified Utlc

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Utlc.main "test.utlc"
