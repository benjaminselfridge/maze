module Maze
  ( -- * Maze type
    IMaze
  , iMazeDims
  , iMazeCanMove
  , iMazeToList
  , Coord
  , neighborCoord
  , Cell
  , cellOpenDown
  , cellOpenRight
  , Direction(..)
    -- * Maze construction algorithms
  , recursiveBacktracking
  , binaryTree
  , kruskal
  ) where

import Maze.Core
import Maze.Algorithms
