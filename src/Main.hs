{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)


main :: IO ()
main = do
  getArgs >>= \case
    [inputFile] -> putStrLn "typechecks."
    _ -> do
      progName <- getProgName
      putStrLn $ "usage: " ++ progName ++ " example-input.csv"
      putStrLn ""
      putStrLn "Select the issues you are interested in and open them in JIRA's issue navigator."
      putStrLn "Enable the 'Key', 'Linked Issues' and 'Story Points' columns."
      putStrLn "Export to csv, using comma as the delimiter."
      putStrLn "Run this program on the resulting csv file, and an ASCII gantt chart will"
      putStrLn "be printed to illustrate the dependencies between all the tasks."
      exitFailure
