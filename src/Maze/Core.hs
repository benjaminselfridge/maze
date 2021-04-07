{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- | This module stores the basic types and low-level operations for building
-- 2-dimensional rectangular mazes. We build the maze in 'ST' and then freeze it
-- when we're done, using mutable and immutable arrays.
module Maze.Core
  ( -- * Cells and directions
    Cell
  , newCell
  , cellOpenRight
  , cellOpenDown
  , Coord
  , Direction(..)
  , flipDirection
  , neighborCoord
    -- * Mutable maze
  , STMaze
  , newSTMaze
  , stMazeDims
  , stMazeInBounds
  , stMazeNeighbors
  , stMazeOpen
    -- * Immutable maze
  , IMaze
  , freezeSTMaze
  , iMazeDims
  , iMazeCanMove
  , iMazeToList
  ) where

import Debug.Trace

import Control.Monad (filterM, when)
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.Bits
import Data.Word

-- | A single cell of a 2-dimensional maze.
data Cell = Cell { cellOpenRight :: Bool
                   -- ^ Is this cell connected to its neighbor on the right?
                 , cellOpenDown :: Bool
                   -- ^ Is this cell connected to its neighbor below?
                 }

-- | Create a fresh cell with both right and down closed.
newCell :: Cell
newCell = Cell False False

-- | The location of a cell within a maze is just a pair @(row, col)@.
type Coord = (Word32, Word32)

-- | Represents a direction relating one cell to another.
data Direction = DUp | DDown | DLeft | DRight
  deriving (Show, Eq)

-- | Get the opposite direction.
flipDirection :: Direction -> Direction
flipDirection DUp    = DDown
flipDirection DDown  = DUp
flipDirection DLeft  = DRight
flipDirection DRight = DLeft

-- | Get the neighbor of a cell in a particular direction. Since we don't check
-- bounds, this can return a coordinate outside the maze (including a negative
-- coordinate), so always use in conjunction with 'stMazeInBounds'.
neighborCoord :: Direction -> Coord -> Coord
neighborCoord dir (r, c) = case dir of
  DUp    -> (r-1,c)
  DDown  -> (r+1,c)
  DLeft  -> (r,c-1)
  DRight -> (r,c+1)

-- | Mutable maze in 'ST' monad.
newtype STMaze s = STMaze { stMazeArray :: STArray s Coord Cell }

-- | Construct a new 'STMaze' with a given number of rows and columns.
newSTMaze :: Word32 -> Word32 -> ST s (STMaze s)
newSTMaze rows cols = STMaze <$> newArray ((0,0),(rows-1,cols-1)) newCell

-- | Get the number of (rows, columns) in an 'STMaze'.
stMazeDims :: STMaze s -> ST s (Word32, Word32)
stMazeDims maze = do
  ((_, _), (hiR, hiC)) <- getBounds (stMazeArray maze)
  return (hiR+1, hiC+1)

-- | Determine if a coordinate lies within an 'STMaze'\'s bounds.
stMazeInBounds :: STMaze s -> Coord -> ST s Bool
stMazeInBounds maze (r, c) = do
  (numRows, numCols) <- stMazeDims maze
  return $
    0 <= r && 0 <= c &&
    r < numRows && c < numCols

-- | Get the cell at a given coordinate of an 'STMaze'. Does not do bounds
-- checking, so this can raise an error.
stMazeGetCell :: STMaze s -> Coord -> ST s Cell
stMazeGetCell maze pos = readArray (stMazeArray maze) pos

-- | Set the cell at a given coordinate of an 'STMaze'. Does not do bounds
-- checking so this can raise an error.
stMazeSetCell :: STMaze s -> Coord -> Cell -> ST s ()
stMazeSetCell maze pos cell = writeArray (stMazeArray maze) pos cell

-- | Get all the neighbors of a particular cell in an 'STMaze', along with their
-- directions relative to the input cell. It doesn't matter whether there is a
-- wall between the cells.
stMazeNeighbors :: STMaze s -> Coord -> ST s [(Direction, Coord)]
stMazeNeighbors maze pos =
  let ns = [ (dir, neighborCoord dir pos) | dir <- [DUp, DDown, DLeft, DRight] ]
  in filterM (stMazeInBounds maze . snd) ns

-- | Open up one of the walls surrounding a cell, given the cell coordinate and
-- the direction of the wall relative to that coordinate. If the direction leads
-- us to a cell outside the maze, silently do nothing.
stMazeOpen :: STMaze s -> Coord -> Direction -> ST s Bool
stMazeOpen maze pos dir = do
  let nPos = neighborCoord dir pos
  inBounds <- stMazeInBounds maze nPos
  when inBounds $ do
    cell <- stMazeGetCell maze pos
    nCell <- stMazeGetCell maze nPos
    case dir of
      DUp    -> stMazeSetCell maze nPos (nCell { cellOpenDown  = True })
      DDown  -> stMazeSetCell maze pos  (cell  { cellOpenDown  = True })
      DLeft  -> stMazeSetCell maze nPos (nCell { cellOpenRight = True })
      DRight -> stMazeSetCell maze pos  (cell  { cellOpenRight = True })
  return inBounds

-- | Immutable maze.
newtype IMaze = IMaze { iMazeArray :: Array Coord Cell }

-- | Freeze a mutable 'STMaze' to an immutable 'IMaze'.
freezeSTMaze :: STMaze s -> ST s IMaze
freezeSTMaze (STMaze a) = IMaze <$> freeze a

-- | Get the number of (rows, columns in an 'IMaze'.
iMazeDims :: IMaze -> (Word32, Word32)
iMazeDims maze = let ((_, _), (hiR, hiC)) = bounds (iMazeArray maze)
                 in (hiR+1, hiC+1)

-- | Determine if a coordinate lies within an 'IMaze'\'s bounds.
iMazeInBounds :: IMaze -> Coord -> Bool
iMazeInBounds maze (r, c) =
  let (numRows, numCols) = iMazeDims maze
  in 0 <= r && 0 <= c &&
     r < numRows && c < numCols

-- | Get the cell at a given coordinate of an 'IMaze'. Does not do bounds
-- checking, so this can raise an error.
iMazeGetCell :: IMaze -> Coord -> Cell
iMazeGetCell maze pos = iMazeArray maze ! pos

-- | Determine if we can move in a given direction from a given cell.
iMazeCanMove :: IMaze -> Coord -> Direction -> Bool
iMazeCanMove maze pos dir
  | nPos <- neighborCoord dir pos
  , iMazeInBounds maze nPos =
    let cell = iMazeGetCell maze pos
        nCell = iMazeGetCell maze nPos
    in case dir of DUp -> cellOpenDown nCell
                   DDown -> cellOpenDown cell
                   DLeft -> cellOpenRight nCell
                   DRight -> cellOpenRight cell
iMazeCanMove _ _ _ = False

-- | Extract a list of lists of cells from an 'IMaze', in row-major order.
iMazeToList :: IMaze -> [[(Coord, Cell)]]
iMazeToList maze = rows
  where (numRows, numCols) = iMazeDims maze
        rows = [ [ ((r,c), iMazeArray maze ! (r,c))
                 | c <- [0..numCols-1] ]
               | r <- [0..numRows-1] ]