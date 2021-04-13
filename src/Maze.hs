module Maze
  ( -- * Maze type
    IMaze
  , iMazeDims
  , iMazeBounds
  , iMazeMove
  , iMazeCoords
  , Coord
  , coordRow
  , coordCol
  , Direction(..)
    -- * Maze construction algorithms
  , recursiveBacktracking
  , binaryTree
  , kruskal
  , prim
  ) where

import Maze.Core
import Maze.Algorithms
