{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.ByteString.Lazy (ByteString)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (for_, toList, foldl') -- Added foldl'
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

----------------------------------------

-- Parse the CSV file into a list of issues.

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

----------------------------------------

-- Add dummy nodes to the graph to make it easier to see the tasks which are
-- ready to start. The dummy nodes are added above the tasks which are
-- unblocked and have the same color as the "Done" tasks, so that all the issues
-- which are ready to start are at the boundary between the "Done" tasks and the
-- non-done tasks.

addDummyEdge :: (IssueId, IssueId) -> Map IssueId Issue -> Map IssueId Issue
addDummyEdge (from, to) = Map.adjust (\issue -> issue {blocks = to : blocks issue}) from

-- The tasks which are ready to start are those whose blocking tasks are
-- completed (marked in green). This makes it difficult to see the tasks with
-- in-degree zero, which are ready to be started but do not have green above
-- them. So let's add a dummy green node above them.
addDummyNodesAndEdges :: Map IssueId Issue -> Map IssueId Issue
addDummyNodesAndEdges g
    = dummyNodes
   <> foldr addDummyEdge g dummyEdges
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

----------------------------------------

-- Convert Issues to Graph{Nodes,Edges}

data GraphNode = GraphNode
  { gnId        :: IssueId
  , gnLabel     :: String
  , gnStyle     :: String
  , gnFillColor :: String
  , gnPenWidth  :: Double
  } deriving (Show)

data GraphEdge = GraphEdge
  { geSource    :: IssueId
  , geTarget    :: IssueId
  , geIsRanking :: Bool
  , geInCycle   :: Bool
  } deriving (Show)


showDouble :: Double -> String
showDouble x
  | fromIntegral (round x) == x
    = show (round x)
  | otherwise
    = show x

toGraphNode :: (IssueId, Issue) -> GraphNode
toGraphNode (issueId, Issue {..}) = GraphNode
  { gnId        = issueId
  , gnLabel     = label
  , gnStyle     = style
  , gnFillColor = color
  , gnPenWidth  = penwidth
  }
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

----------------------------------------

-- Cycle Detection Logic

-- Helper to reconstruct cycle edges given a back-edge u -> v (v is an ancestor)
reconstructCycleFromBackEdge :: IssueId -> IssueId -> Map IssueId IssueId -> Set (IssueId, IssueId)
reconstructCycleFromBackEdge u v parentMap =
  let backEdge = Set.singleton (u,v)
      pathEdges = go u Set.empty
  in Set.union backEdge pathEdges
  where
    go curr ancestorAccEdges
      | curr == v = ancestorAccEdges -- Reached the start of the cycle path (the ancestor v)
      | otherwise =
          case Map.lookup curr parentMap of
            Nothing -> ancestorAccEdges -- Should not happen if v is an ancestor of curr in a connected path
            Just p  -> go p (Set.insert (p, curr) ancestorAccEdges)

findEdgesInCycles :: Set IssueId -> Map IssueId (Set IssueId) -> Set (IssueId, IssueId)
findEdgesInCycles allGraphNodes adjMap =
  fst $ Set.foldl' visitStartNode (Set.empty, Set.empty) allGraphNodes
  where
    -- visitStartNode is called for each node in the graph to ensure all components are visited.
    -- State: (accumulated cycle edges, globally visited nodes)
    visitStartNode (accCycleEdges, visitedGlobally) node
      | Set.member node visitedGlobally = (accCycleEdges, visitedGlobally)
      | otherwise =
          let (newCycleEdges, visitedInComponent) = dfs node Set.empty visitedGlobally Map.empty
          in (Set.union accCycleEdges newCycleEdges, Set.union visitedGlobally visitedInComponent)

    -- dfs :: current_node -> visiting_set (recursion stack) -> visited_globally_set -> parent_map -> (Set (IssueId, IssueId), Set IssueId)
    -- Returns: (edges found in cycles during this DFS path, all nodes visited starting from this DFS root)
    dfs u visiting visitedGlobally parents =
      let visiting' = Set.insert u visiting
          visitedGlobally' = Set.insert u visitedGlobally

          (foundCycleEdgesInNeighbors, finalVisited) = foldl'
            (\(accCycles, visitedAcc) v ->
              if Set.member v visiting' then -- Cycle detected (back edge u -> v)
                let cycleEdges = reconstructCycleFromBackEdge u v parents
                in (Set.union accCycles cycleEdges, visitedAcc)
              else if Set.notMember v visitedGlobally' then
                let (newCyclesFromV, visitedByV) = dfs v visiting' visitedAcc (Map.insert v u parents)
                in (Set.union accCycles newCyclesFromV, visitedByV) -- Important: visitedByV contains all nodes visited from v's DFS
              else
                (accCycles, visitedAcc) -- v already visited and not in current path
            )
            (Set.empty, visitedGlobally') -- Initial accumulator for fold over neighbors
            (Map.findWithDefault Set.empty u adjMap)
      in (foundCycleEdgesInNeighbors, finalVisited)

markCycleEdges :: ([GraphNode], [GraphEdge]) -> (Bool, [GraphNode], [GraphEdge])
markCycleEdges (graphNodes, graphEdges) = (foundCycles, graphNodes, finalGraphEdges)
  where
    -- Build adjacency list and get all nodes for cycle detection
    adjMap :: Map IssueId (Set IssueId)
    adjMap = Map.fromListWith Set.union $ do
        GraphEdge {geSource, geTarget} <- graphEdges
        pure (geSource, Set.singleton geTarget)

    allNodesFromGraphEdges :: Set IssueId
    allNodesFromGraphEdges
      = Set.fromList
      $ concatMap
          (\(GraphEdge {geSource, geTarget}) -> [geSource, geTarget])
          graphEdges

    allNodesFromGraphNodes :: Set IssueId
    allNodesFromGraphNodes = Set.fromList $ map gnId graphNodes

    comprehensiveNodesSet :: Set IssueId
    comprehensiveNodesSet = Set.union allNodesFromGraphEdges allNodesFromGraphNodes

    edgesInCycles :: Set (IssueId, IssueId)
    edgesInCycles = findEdgesInCycles comprehensiveNodesSet adjMap

    foundCycles :: Bool
    foundCycles = not $ Set.null edgesInCycles

    -- Update attributes for edges in cycles
    finalGraphEdges :: [GraphEdge]
    finalGraphEdges = map (\edge@(GraphEdge {geSource, geTarget}) ->
                              if Set.member (geSource, geTarget) edgesInCycles
                              then edge { geInCycle = True }
                              else edge
                          ) graphEdges

toGraphNodesAndEdges :: Map IssueId Issue -> [(IssueId, IssueId)] -> ([GraphNode], [GraphEdge])
toGraphNodesAndEdges issues rankingPairs = (graphNodes, allGraphEdges)
  where
    graphNodes :: [GraphNode]
    graphNodes = fmap toGraphNode (Map.toList issues)

    dependencyGraphEdges :: [GraphEdge]
    dependencyGraphEdges = concatMap
        (\ (issueId, issueDetails) ->
            [ GraphEdge
                { geSource    = issueId
                , geTarget    = blockedId
                , geIsRanking = False
                , geInCycle   = False
                }
            | blockedId <- blocks issueDetails
            ]
        )
        (Map.toList issues)

    rankingGraphEdges :: [GraphEdge]
    rankingGraphEdges = fmap
        (\(id1, id2) ->
            GraphEdge
              { geSource    = id1
              , geTarget    = id2
              , geIsRanking = True
              , geInCycle   = False
              }
        )
        rankingPairs

    allGraphEdges :: [GraphEdge]
    allGraphEdges = dependencyGraphEdges ++ rankingGraphEdges

----------------------------------------

-- Simpler API for creating a DotGraph

mkNodeId :: String -> Dot.NodeId
mkNodeId = fromString

mkDotNode :: String -> [Dot.Attribute] -> Dot.NodeStatement
mkDotNode name attrs = Dot.NodeStatement (mkNodeId name) attrs

mkDotEdge :: String -> String -> [Dot.Attribute] -> Dot.EdgeStatement
mkDotEdge name1 name2 attrs
  = Dot.EdgeStatement
      (Dot.ListTwo
        (Dot.EdgeNode (mkNodeId name1))
        (Dot.EdgeNode (mkNodeId name2))
        [])
      attrs

mkDotGraph :: [Dot.NodeStatement] -> [Dot.EdgeStatement] -> Dot.DotGraph
mkDotGraph nodes edges
  = Dot.DotGraph
      Dot.Strict
      Dot.Directed
      Nothing
      ( (Dot.StatementNode <$> nodes)
     ++ (Dot.StatementEdge <$> edges)
      )

printDotGraph :: Dot.DotGraph -> IO ()
printDotGraph = Text.putStrLn . Dot.encode

----------------------------------------

-- Convert the Graph{Nodes,Edges} to a DotGraph

toDotNode :: GraphNode -> Dot.NodeStatement
toDotNode (GraphNode {..}) = mkDotNode gnId attrs
  where
    attrs :: [Dot.Attribute]
    attrs = [ Dot.Attribute "label" (fromString gnLabel)
            , Dot.Attribute "style" (fromString ("filled," ++ gnStyle))
            , Dot.Attribute "fillcolor" (fromString gnFillColor)
            , Dot.Attribute "penwidth" (fromString (show gnPenWidth))
            ]

toDotEdge :: Bool -> GraphEdge -> Dot.EdgeStatement
toDotEdge True (GraphEdge {..}) = mkDotEdge geSource geTarget attrs
  where
    attrs :: [Dot.Attribute]
    attrs = (if geIsRanking then [Dot.Attribute "style" (fromString "dotted")] else [])
         ++ (if geInCycle then [Dot.Attribute "color" (fromString "red")] else [])
toDotEdge False (GraphEdge {..}) = mkDotEdge geSource geTarget attrs
  where
    attrs :: [Dot.Attribute]
    attrs = [Dot.Attribute "style" (fromString "invis")]


toDotGraph :: Bool -> [GraphNode] -> [GraphEdge] -> Dot.DotGraph
toDotGraph displayRankingEdges graphNodes graphEdges
  = mkDotGraph dotNodes dotEdges
  where
    dotNodes :: [Dot.NodeStatement]
    dotNodes = fmap toDotNode graphNodes

    dotEdges :: [Dot.EdgeStatement]
    dotEdges = fmap (toDotEdge displayRankingEdges) graphEdges

----------------------------------------

main :: IO ()
main = do
  getArgs >>= \case
    [inputFile] -> do
      inputCsv <- ByteString.readFile inputFile
      let orderedIssues = parseIssues inputCsv
      let (rankingEdges, issuesMap) = extractRankingAndMap orderedIssues
      let issuesMap' = addDummyNodesAndEdges issuesMap
      let (graphNodes, graphEdges) = toGraphNodesAndEdges issuesMap' rankingEdges
      let (foundCycles, graphNodes', graphEdges') = markCycleEdges (graphNodes, graphEdges)
      let dotGraph = toDotGraph foundCycles graphNodes' graphEdges'
      printDotGraph dotGraph
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
