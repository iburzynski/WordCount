module Main where

import System.Environment
import Lib

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname, num, stops] -> do
      fname' <- validateFile fname
      count  <- validateCount num
      processTextFile fname' count stops
    [fname, num] -> do
      fname' <- validateFile fname
      count  <- validateCount num
      processTextFile fname' count ""
    [fname] -> do
      fname' <- validateFile fname
      c <- requestCount
      processTextFile fname c ""
    _ -> do
      fname <- requestFile
      c <- requestCount
      processTextFile fname c ""