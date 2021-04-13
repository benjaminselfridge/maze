module Maze.Algorithms.Kruskal
  ( kruskal
  ) where

import Maze.Core
import Maze.Utils

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad (forM_, forM)
import Control.Monad.ST
import Data.Ix
import Data.STRef
import Data.Tuple
import Data.UnionFind.ST
import Data.Word
import System.Random

-- | Build a maze using Kruskal's algorithm.
kruskal :: RandomGen g => g -> Word32 -> Word32 -> (IMaze, g)
kruskal g numRows numCols = runST $ do
  maze <- newSTMaze numRows numCols
  edges <- stMazeInnerEdges maze
  let (edges', g') = shuffle edges g
  k <- newKruskal maze
  forM_ edges' $ \e -> do
    let (pos, pos') = edgeNeighbors e
    sameSet <- kruskalSameSet k pos pos'
    case sameSet of
      True -> return ()
      False -> do
        stMazeOpenEdge maze e
        kruskalUnion k pos pos'
  imaze <- freezeSTMaze maze
  return (imaze, g')

type Id = Word32

-- | Bookkeeping data structure for Kruskal's algorithm.
data K s = K { coordPoint :: Map.Map Coord (Point s Coord) }

-- | Create a fresh 'Kruskal' where each coord is in a singleton set with a
-- distinct id.
newKruskal :: STMaze s -> ST s (K s)
newKruskal maze = do
  mazeBounds <- stMazeBounds maze
  let coords = range mazeBounds
  pairs <- forM coords $ \pos -> do
    pt <- fresh pos
    return (pos, pt)
  return $ K (Map.fromList pairs)

kruskalSameSet :: K s -> Coord -> Coord -> ST s Bool
kruskalSameSet k pos pos' =
  (coordPoint k Map.! pos) `equivalent` (coordPoint k Map.! pos')

kruskalUnion :: K s -> Coord -> Coord -> ST s ()
kruskalUnion k pos pos' = (coordPoint k Map.! pos) `union` (coordPoint k Map.! pos')
