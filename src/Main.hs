{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Main where

import Data.ByteString.Lazy (ByteString)
import Data.Foldable (for_, toList)
import Data.Map (Map)
import Data.Maybe (fromJust, fromMaybe)
import Data.String (fromString)
import Data.Vector (Vector)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import Text.Read (readMaybe)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Csv as Csv
import qualified Data.Map as Map
import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector
import qualified Dot


type IssueId = String

data Issue = Issue
  { summary     :: Maybe String
  , blocks      :: [IssueId]
  , storyPoints :: Maybe Double
  , status      :: Maybe String
  }
  deriving Show

-- partial if the input not valid csv or doesn't contain the required field "Issue key".
-- fields "Summary", "Outward issue link (Blocks)" and "Custom field (Story Points)" are
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
                                       , Issue (getSummary annotatedRow)
                                               (getBlocks annotatedRow)
                                               (getStoryPoints annotatedRow)
                                               (getStatus annotatedRow)
                                       )

    getIssueId :: [(String, String)] -> IssueId
    getIssueId = fromJust
               . lookup "Issue key"

    getSummary :: [(String, String)] -> Maybe String
    getSummary = lookup "Summary"

    getBlocks :: [(String, String)] -> [IssueId]
    getBlocks = filter (not . null)
              . fmap snd
              . filter ((== "Outward issue link (Blocks)") . fst)

    getStoryPoints :: [(String, String)] -> Maybe Double
    getStoryPoints fields = do
      x <- lookup "Custom field (Story Points)" fields
      readMaybe x

    getStatus :: [(String, String)] -> Maybe String
    getStatus = lookup "Status"


-- Simpler API for creating a DotGraph

mkNodeId :: String -> Dot.NodeId
mkNodeId = fromString

mkNode :: String -> Dot.NodeStatement
mkNode name = Dot.NodeStatement (mkNodeId name) []

mkLabelledNode :: String -> String -> String -> Dot.NodeStatement
mkLabelledNode name label color = Dot.NodeStatement (mkNodeId name)
                                    [ Dot.Attribute "label" (fromString label)
                                    , Dot.Attribute "style" (fromString "filled")
                                    , Dot.Attribute "fillcolor" (fromString color)
                                    ]

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

showDouble :: Double -> String
showDouble x
  | fromIntegral (round x) == x
    = show (round x)
  | otherwise
    = show x

toNode :: (IssueId, Issue) -> Dot.NodeStatement
toNode (issueId, Issue {..}) = mkLabelledNode issueId label color
  where
    label = unlines
      $ [unwords ( [issueId]
                ++ ["(" ++ showDouble x ++ ")" | x <- toList storyPoints]
                 )]
     ++ [x | x <- toList summary]

    color = case status of
      Nothing
        -> "white"
      Just "To Do"
        -> "white"
      Just "Done"
        -> "darkolivegreen1"
      Just "Won't Fix"
        -> "gainsboro"
      Just _  -- probably "In Progress" or "QA"
        -> "#f1ffdb"  -- between white and darkolivegreen1

toEdges :: (IssueId, Issue) -> [Dot.EdgeStatement]
toEdges (issue1, Issue {..}) = [mkEdge issue1 issue2 | issue2 <- blocks]

toGraph :: Map IssueId Issue -> Dot.DotGraph
toGraph issues = mkGraph nodes edges
  where
    nodes = fmap toNode (Map.toList issues)
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
