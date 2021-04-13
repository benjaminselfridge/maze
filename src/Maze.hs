module Maze
  ( -- * Maze type
    IMaze
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
  ) where

import Maze.Core
import Maze.Algorithms
