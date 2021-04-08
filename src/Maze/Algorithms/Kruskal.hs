module Maze.Algorithms.Kruskal
  ( kruskal
  , newKruskal
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
  edges <- stMazeEdgeIds maze
  let (edges', g') = shuffle edges g
  k <- newKruskal numRows numCols
  forM_ edges' $ \e -> do
    let pos     = edgeIdCoord e
        edgeDir = edgeIdDirection e
        nPos    = neighborCoord edgeDir pos
    nPosInBounds <- stMazeInBounds maze nPos
    case nPosInBounds of
      True -> do
        sameSet <- kruskalSameSet k pos nPos
        case sameSet of
          True -> return ()
          False -> do
            stMazeOpen maze pos edgeDir
            kruskalUnion k pos nPos
      False -> return ()
  imaze <- freezeSTMaze maze
  return (imaze, g')

type Id = Word32

-- | Bookkeeping data structure for Kruskal's algorithm.
data K s = K { coordPoint :: Map.Map Coord (Point s Coord) }

-- | Create a fresh 'Kruskal' where each coord is in a singleton set with a
-- distinct id.
newKruskal :: Word32 -> Word32 -> ST s (K s)
newKruskal rows cols = do
  let coords = range ((0,0), (rows-1,cols-1))
  pairs <- forM coords $ \pos -> do
    pt <- fresh pos
    return (pos, pt)
  return $ K (Map.fromList pairs)

kruskalSameSet :: K s -> Coord -> Coord -> ST s Bool
kruskalSameSet k pos pos' =
  (coordPoint k Map.! pos) `equivalent` (coordPoint k Map.! pos')

kruskalUnion :: K s -> Coord -> Coord -> ST s ()
kruskalUnion k pos pos' = (coordPoint k Map.! pos) `union` (coordPoint k Map.! pos')
