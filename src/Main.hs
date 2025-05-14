{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Main where

import Data.ByteString.Lazy (ByteString)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (for_, toList)
import Data.Map (Map, (!))
import Data.Set (Set)
import Data.Maybe (fromJust, fromMaybe)
import Data.String (fromString)
import Data.Vector (Vector)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import Text.Read (readMaybe)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Csv as Csv
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector
import qualified Dot


type IssueId = String

data Issue = Issue
  { summary     :: Maybe String
  , blocks      :: [IssueId]
  , storyPoints :: Maybe Double
  , status      :: Maybe String
  , issueType   :: Maybe String
  , labels      :: [String]
  , dummy       :: Bool
  }
  deriving Show

-- partial if the input not valid csv or doesn't contain the required field "Issue key".
-- fields "Summary", "Outward issue link (Blocks)" and "Custom field (Story Points)" are
-- treated specially, but are not mandatory.
parseIssues :: ByteString -> [(IssueId, Issue)]
parseIssues inputCsv = fmap parseAnnotatedIssue
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
                                               (getIssueType annotatedRow)
                                               (getLabels annotatedRow)
                                               False
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

    getIssueType :: [(String, String)] -> Maybe String
    getIssueType = lookup "Issue Type"

    getLabels :: [(String, String)] -> [String]
    getLabels = fmap snd
              . filter ((== "Labels") . fst)


-- The ranking is a list of invisible edges which cause the nodes to be
-- displayed in rank order from top to bottom.
extractRankingAndMap :: [(IssueId, Issue)] -> ([(IssueId, IssueId)], Map IssueId Issue)
extractRankingAndMap orderedIssues = (rankingEdges, issuesMap)
  where
    issuesMap = Map.fromList orderedIssues
    issueIds = fmap fst orderedIssues
    -- Create pairs of (prevIssue, currentIssue) for ranking
    rankingEdges = if length issueIds < 2
                   then []
                   else zip issueIds (tail issueIds)


-- Simpler API for creating a DotGraph

mkNodeId :: String -> Dot.NodeId
mkNodeId = fromString

mkNode :: String -> Dot.NodeStatement
mkNode name = Dot.NodeStatement (mkNodeId name) []

mkLabelledNode :: String -> String -> String -> String -> Double -> Dot.NodeStatement
mkLabelledNode name label color style penwidth
  = Dot.NodeStatement (mkNodeId name)
      [ Dot.Attribute "label" (fromString label)
      , Dot.Attribute "style" (fromString ("filled," ++ style))
      , Dot.Attribute "fillcolor" (fromString color)
      , Dot.Attribute "penwidth" (fromString (show penwidth))
      ]

mkEdge :: String -> String -> [Dot.Attribute] -> Dot.EdgeStatement
mkEdge name1 name2 attrs = Dot.EdgeStatement
                       (Dot.ListTwo
                         (Dot.EdgeNode (mkNodeId name1))
                         (Dot.EdgeNode (mkNodeId name2))
                         [])
                         attrs

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
toNode (issueId, Issue {..}) = mkLabelledNode issueId label color style penwidth
  where
    label
      | dummy
        = ""
      | otherwise
        = unlines
        $ [unwords ( [issueId]
                  ++ ["(" ++ showDouble x ++ ")" | x <- toList storyPoints]
                   )]
       ++ maybe [] (wrapText 30) summary

    wrapText :: Int -> String -> [String]
    wrapText charLimit = go [] . words
      where
        go :: [String] -> [String] -> [String]
        go [] [] = []
        go line [] = [unwords line]
        go [] (word:words)
          = go [word] words
        go line (word:words)
          = let line' = line ++ [word]
                charLength = length (unwords line')
            in if charLength > charLimit
               then unwords line : go [] (word:words)
               else go line' words

    color = case status of
      Nothing
        -> "white"
      Just "ToDo"
        -> "white"
      Just "Done"
        -> "darkolivegreen1"
      Just "Won't Fix"
        -> "gainsboro"
      Just _  -- probably "In Progress" or "QA"
        -> "#f1ffdb"  -- between white and darkolivegreen1

    style = if "stretch-goal" `elem` labels || issueType == Just "Tech Debt"
            then "dashed"
            else "solid"

    penwidth = case issueType of
      Just "Story"
        -> 3.0
      _ -> 1.0

mkDependencyEdge :: (IssueId, IssueId) -> Dot.EdgeStatement
mkDependencyEdge (id1, id2)
  = mkEdge id1 id2 []

mkRankingEdge :: (IssueId, IssueId) -> Dot.EdgeStatement
mkRankingEdge (id1, id2)
  = mkEdge id1 id2 [Dot.Attribute "style" (fromString "dotted")]

toDependencyEdges :: (IssueId, Issue) -> [Dot.EdgeStatement]
toDependencyEdges (issue1, Issue {..})
  = [mkDependencyEdge (issue1, issue2) | issue2 <- blocks]

toGraph :: Map IssueId Issue -> [(IssueId, IssueId)] -> Dot.DotGraph
toGraph issues rankingPairs = mkGraph nodes allEdges
  where
    nodes = fmap toNode (Map.toList issues)
    dependencyEdges = concatMap toDependencyEdges (Map.toList issues)
    rankingEdges = fmap mkRankingEdge rankingPairs
    allEdges = dependencyEdges ++ rankingEdges


addEdge :: (IssueId, IssueId) -> Map IssueId Issue -> Map IssueId Issue
addEdge (from, to) = Map.adjust (\issue -> issue {blocks = to : blocks issue}) from

-- The tasks which are ready to start are those whose blocking tasks are
-- completed (marked in green). This makes it difficult to see the tasks with
-- in-degree zero, which are ready to be started but do not have green above
-- them. So let's add a dummy green node above them.
markInitialNodes :: Map IssueId Issue -> Map IssueId Issue
markInitialNodes g = dummyNodes
                  <> foldr addEdge g dummyEdges
  where
    potentialNodes :: Map IssueId Issue
    potentialNodes = Map.filter (\(Issue {..}) -> status `elem` [Nothing, Just "ToDo"]) g

    blockedNodes :: Set IssueId
    blockedNodes = Set.unions $ fmap (Set.fromList . blocks) g

    initialNodes :: Map IssueId Issue
    initialNodes = potentialNodes `Map.withoutKeys` blockedNodes

    mkDummyId :: IssueId -> IssueId
    mkDummyId = ("before-" ++)

    mkDummyNode :: (IssueId, Issue) -> (IssueId, Issue)
    mkDummyNode (issueId, Issue {..})
      = ( mkDummyId issueId
        , Issue
          { summary     = Nothing
          , blocks      = [issueId]
          , storyPoints = Nothing
          , status      = Just "Done"
          , issueType   = Nothing
          , labels      = labels
          , dummy       = True
          }
        )

    mkDummyEdge :: IssueId -> (IssueId, IssueId)
    mkDummyEdge issueId = (mkDummyId issueId, issueId)

    dummyNodes :: Map IssueId Issue
    dummyNodes = Map.fromList $ fmap mkDummyNode $ Map.toList initialNodes

    dummyEdges :: [(IssueId, IssueId)]
    dummyEdges = fmap mkDummyEdge $ Map.keys initialNodes

main :: IO ()
main = do
  getArgs >>= \case
    [inputFile] -> do
      inputCsv <- ByteString.readFile inputFile
      let orderedIssues = parseIssues inputCsv
      let (rankingEdges, issuesMap) = extractRankingAndMap orderedIssues
      let finalIssuesMap = markInitialNodes issuesMap
      let graph = toGraph finalIssuesMap rankingEdges
      printGraph graph
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
