module Maze.Algorithms.Prim
  ( prim
  ) where

import Maze.Core
import Maze.Utils

import Control.Monad.ST
import qualified Data.Map as Map
import qualified Data.PQueue.Min as PQ
import Data.Word
import System.Random

data Entry = Entry Int Wall

instance Eq Entry where
  Entry a _ == Entry b _ = a == b

instance Ord Entry where
  Entry a _ `compare` Entry b _ = a `compare` b

-- | Build a maze using Prim's algorithm.
prim :: RandomGen g => g -> Word32 -> Word32 -> (IMaze, g)
prim g numRows numCols = runST $ do
  maze <- newSTMaze numRows numCols
  -- walls <- stMazeWalls maze
  -- let (walls', g') = shuffle walls g
  --     entryWeightMap = Map.fromList $ zip walls' [1..]
  undefined
