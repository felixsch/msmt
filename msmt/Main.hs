module Main where

import           Cli

main :: IO ()
main = commandline $ do
  options <- parseOptions
  putStrLn $ "Options are: " ++ show options
