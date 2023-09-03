{-# LANGUAGE FlexibleContexts #-}

module Main where

import AsciiMath
import System.Environment (getArgs)
import System.Directory (doesFileExist)

main :: IO ()
main = do
  args <- getArgs
  let specFile =
        case args of
          [] -> "math.conf"
          f:_args -> f
  fileExists <- doesFileExist specFile
  if fileExists
    then interact . translate =<< specFromFile specFile
    else putStrLn "No configuration file was provided."