{-# LANGUAGE LambdaCase #-}
module Main where

import Data.ByteString.Lazy (ByteString)
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Maybe (fromJust, fromMaybe)
import Data.Vector (Vector)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Csv as Csv
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import AsciiArt


type IssueId = String

data Issue = Issue
  { blocks      :: [IssueId]
  , storyPoints :: Double
  }
  deriving Show

-- partial if the input not valid csv or doesn't contain the required field "Issue key".
-- fields "Outward issue link (Blocks)" and "Custom field (Story Points)" are
-- treated specially, but are not mandatory.
parseIssues :: ByteString -> Map IssueId Issue
parseIssues inputCsv = Map.fromList
                  . fmap parseAnnotatedIssue
                  $ annotatedRows
  where
    annotatedRows :: [[(String, String)]]
    annotatedRows = case fmap Vector.toList (Csv.decode Csv.NoHeader inputCsv) of
      Left err -> error err
      Right (header:rows) -> fmap (zip header) rows

    parseAnnotatedIssue :: [(String, String)] -> (IssueId, Issue)
    parseAnnotatedIssue annotatedRow = ( getIssueId annotatedRow
                                       , Issue (getBlocks annotatedRow)
                                               (getStoryPoints annotatedRow)
                                       )

    getIssueId :: [(String, String)] -> IssueId
    getIssueId = fromJust
               . lookup "Issue key"

    getBlocks :: [(String, String)] -> [IssueId]
    getBlocks = filter (not . null)
              . fmap snd
              . filter ((== "Outward issue link (Blocks)") . fst)

    getStoryPoints :: [(String, String)] -> Double
    getStoryPoints = read
                   . fromMaybe "0"
                   . lookup "Custom field (Story Points)"

main :: IO ()
main = do
  getArgs >>= \case
    [inputFile] -> do
      issues <- parseIssues <$> ByteString.readFile inputFile
      print issues
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
