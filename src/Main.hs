{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Main where

import Data.ByteString.Lazy (ByteString)
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Maybe (fromJust, fromMaybe)
import Data.String (fromString)
import Data.Vector (Vector)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Csv as Csv
import qualified Data.Map as Map
import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector
import qualified Dot


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


-- Simpler API for creating a DotGraph

mkNodeId :: String -> Dot.NodeId
mkNodeId = fromString

mkNode :: String -> Dot.NodeStatement
mkNode name = Dot.NodeStatement (mkNodeId name) []

mkEdge :: String -> String -> Dot.EdgeStatement
mkEdge name1 name2 = Dot.EdgeStatement
                       (Dot.ListTwo
                         (Dot.EdgeNode (mkNodeId name1))
                         (Dot.EdgeNode (mkNodeId name2))
                         [])
                         []

mkGraph :: [Dot.NodeStatement] -> [Dot.EdgeStatement] -> Dot.DotGraph
mkGraph nodes edges = Dot.DotGraph
                        Dot.Strict
                        Dot.Directed
                        Nothing
                        ( (Dot.StatementNode <$> nodes)
                       ++ (Dot.StatementEdge <$> edges)
                        )

printGraph :: Dot.DotGraph -> IO ()
printGraph = Text.putStrLn . Dot.encode


-- Convert Issues to a DotGraph

toNode :: IssueId -> Dot.NodeStatement
toNode = mkNode

toEdges :: (IssueId, Issue) -> [Dot.EdgeStatement]
toEdges (issue1, Issue {..}) = [mkEdge issue1 issue2 | issue2 <- blocks]

toGraph :: Map IssueId Issue -> Dot.DotGraph
toGraph issues = mkGraph nodes edges
  where
    nodes = fmap toNode (Map.keys issues)
    edges = concatMap toEdges (Map.toList issues)


main :: IO ()
main = do
  getArgs >>= \case
    [inputFile] -> do
      issues <- parseIssues <$> ByteString.readFile inputFile
      printGraph $ toGraph issues
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
