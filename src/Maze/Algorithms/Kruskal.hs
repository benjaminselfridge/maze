module Maze.Algorithms.Kruskal
  ( kruskal
  , newKruskal
  ) where

import Maze.Core

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Ix
import Data.Tuple
import Data.Word
import System.Random

kruskal :: RandomGen g => g -> Word32 -> Word32 -> (IMaze, g)
kruskal g numRows numCols = undefined

type Id = Word32

-- | Bookkeeping data structure for Kruskal's algorithm.
data Kruskal = Kruskal { coordIds :: Map.Map Coord Id
                       , idCoords :: Map.Map Id (Set.Set Coord)
                       }
  deriving Show

-- | Create a fresh 'Kruskal' where each coord is in a singleton set with a
-- distinct id.
newKruskal :: Word32 -> Word32 -> Kruskal
newKruskal rows cols =
  let coords = range ((0,0), (rows-1,cols-1))
      coordSets = Set.singleton <$> coords
      ids = [0..]
  in Kruskal (Map.fromList (zip coords ids)) (Map.fromList (zip ids coordSets))
