module Fao.Pathfinding ( BoardMap
                       , HeroBoardMap
                       , buildSafeBoardMap
                       , buildBoardMap
                       , buildHeroBoardMap
                       , buildSafeHeroBoardMap
                       , distance
                       , walk
                       ) where

import Data.List (unfoldr, foldl')
import Data.Maybe (fromMaybe)
import Data.PSQueue (PSQ, Binding(..))
import Control.Applicative ((<$>))
import Control.Monad (unless, forM_)
import Control.Monad.State (execState, get, modify)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.PSQueue as PSQ

import Fao.Types
import Fao.Utils

buildHeroBoardMap :: Vindinium -> HeroBoardMap
buildHeroBoardMap state =
    let heroes = gameHeroes $ vindiniumGame state
        constructBoardMap m hero =
          let boardMap = buildBoardMap state hero
          in M.insert hero boardMap m
    in HeroBoardMap $ foldl' constructBoardMap M.empty heroes

buildSafeHeroBoardMap :: Vindinium -> HeroBoardMap
buildSafeHeroBoardMap state =
    let heroes = gameHeroes $ vindiniumGame state
        constructSafeBoardMap m hero =
          let safeBoardMap = buildSafeBoardMap state hero
          in M.insert hero safeBoardMap m
    in HeroBoardMap $ foldl' constructSafeBoardMap M.empty heroes

buildBoardMap :: Vindinium -> Hero -> BoardMap
buildBoardMap state hero =
    let start = heroPos hero
    in pathDijkstra (dijkstra (adjacent state start) manhattan start)

buildSafeBoardMap :: Vindinium -> Hero -> BoardMap
buildSafeBoardMap state hero =
    let start = heroPos hero
    in pathDijkstra (dijkstra (adjacentSafe state start) manhattan start)

distance :: Path -> Int
distance (Path path) = length path

walk :: Hero -> Path -> Dir
walk _ (Path []) = Stay
walk hero (Path (nextPos : _)) = dirFromPos (heroPos hero) nextPos

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- Based on https://gist.github.com/kazu-yamamoto/5218431
--          http://mew.org/~kazu/material/2012-psq.pdf

type Graph    = Pos -> [Pos]             -- Given a position on a board return all neighbours
type Cost     = Int                      -- Connected tiles on a board all have cost 1
type Vertex   = Pos                      -- Nodes in the graph are represented by position on a board
type Queue    = PSQ Vertex Priority      -- Priority search queue to be used in constructing shortest paths from starting position
type Mapping  = (Vertex, Priority)       -- Every node is going to be assigned a priority

-- keeps information from which node we got to the node we are interested in
newtype Dijkstra = Dijkstra (M.Map Vertex Vertex)

data Priority = Priority Cost Vertex deriving (Eq)

instance Ord Priority where
    Priority cost1 _ `compare` Priority cost2 _ = cost1 `compare` cost2

-- select neighbour positions which we can actually move to
-- taking into account the position from which we are asking those
-- neighbouring tiles (if it is not a FreeTile or tile at Hero position
-- then we cannot possibly reach that tile and there is no way to go to any
-- of its neighbours)
--
-- Example: given a 1x3 board [Hero, WoodTile, MineTile]
-- if we ask to give neighbours of position (1,2) then we would return an
-- empty set because we cannot actually make our hero go to (1,2) in the
-- first place. If we didn't restrict it then it would return the
-- neighbouring tiles and our pathing would think that it can reach MineTile
adjacent :: Vindinium -> Vertex -> Graph
adjacent state start pos =
    let board = gameBoard $ vindiniumGame state
        tiles = S.toList $ adjacentTiles board pos
        canEnter tile =
          let targetTile = fromMaybe WoodTile (tileAt board tile)
          in targetTile /= WoodTile
    in if pos == start || fromMaybe WoodTile (tileAt board pos) == FreeTile
          then filter canEnter tiles
          else []

-- extra precaution, we avoid enemies during our path construction
adjacentSafe :: Vindinium -> Vertex -> Graph
adjacentSafe state start pos =
    let board = gameBoard $ vindiniumGame state
        tiles = S.toList $ adjacentTiles board pos
        heroHasEnoughHealth = heroLife (vindiniumHero state) > 30
        worthEntering tile =
          let targetTile = fromMaybe WoodTile (tileAt board tile)
          in if heroHasEnoughHealth
               then targetTile /= WoodTile
               else targetTile /= WoodTile && not (isEnemyNearby state pos)
    in if pos == start || fromMaybe WoodTile (tileAt board pos) == FreeTile
         then filter worthEntering tiles
         else []

-- given a start position construct shortest paths to all other positions
dijkstra :: Graph -> Distance -> Vertex -> Dijkstra
dijkstra graph dist start = buildDijkstra $ unfoldr (step graph dist) $ relax (start, Priority 0 start) queue
  where
    queue = PSQ.fromList $ map (\v -> v :-> Priority maxBound start) (S.toList $ vertices graph start)
    buildDijkstra = foldl' insertEdge (Dijkstra M.empty)
    insertEdge dij@(Dijkstra p) (destination, _, source) = if source /= destination then Dijkstra (M.insert destination source p) else dij

-- construct next edge in the graph taking the vertex with minimal cost
step :: Graph -> Distance -> Queue -> Maybe ((Vertex, Cost, Vertex), Queue)
step graph dist queue = extractMin graph dist <$> PSQ.minView queue

-- grab an item from PSQueue with minimal priority
-- and update priority of its neighbours
extractMin :: Graph -> Distance -> (Binding Vertex Priority, Queue) -> ((Vertex, Cost, Vertex), Queue)
extractMin graph dist (destination :-> Priority cost source, queue) = ((destination, cost, source), relaxList queue adj)
  where
    adj = [(neighbour, Priority (cost + dist destination neighbour) destination) | neighbour <- graph destination]

-- update priority of a given key in PSQueue
relax :: Mapping -> Queue -> Queue
relax (key, priority@(Priority cost _)) = PSQ.adjust update key
  where
    update otherPriority@(Priority otherCost _)
      | cost < otherCost = priority
      | otherwise = otherPriority

-- update priorities of given keys in PSQueue
relaxList :: Queue -> [Mapping] -> Queue
relaxList = foldr relax

-- collect all unique vertices from graph
vertices :: Graph -> Vertex -> S.Set Vertex
vertices graph start = execState (visitFrom start) S.empty
  where
    visitFrom start' =
      forM_ (graph start') $ \neighbour ->
        get >>= \v -> unless (neighbour `S.member` v) $ modify (S.insert neighbour) >> visitFrom neighbour

-- path to a goal position
pathDijkstra :: Dijkstra -> Vertex -> Maybe Path
pathDijkstra (Dijkstra prev) goal =
    case searchBackwards goal of
      [_] -> Nothing
      path -> Just (Path $ (tail . reverse) path)
    where
      searchBackwards goal' = goal' : maybe [] searchBackwards (M.lookup goal' prev)
