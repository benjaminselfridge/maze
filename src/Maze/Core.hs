{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- | This module stores the basic types and low-level operations for building
-- 2-dimensional rectangular mazes. We build the maze in 'ST' and then freeze it
-- when we're done, using mutable and immutable arrays.
module Maze.Core
  ( -- * Coordinates, directions, and edges
    Coord
  , coordRow
  , coordCol
  , EdgeId
  , edgeIdNeighbors
  , edgeIdDirection
  , Direction(..)
    -- * Mutable maze
  , STMaze
  , newSTMaze
  , stMazeDims
  , stMazeBounds
  , stMazeInnerEdges
  , stMazeNeighborCoords
  , stMazeOpen
  , stMazeOpenEdge
    -- * Immutable maze
  , IMaze
  , freezeSTMaze
  , iMazeDims
  , iMazeBounds
  , iMazeMove
  , iMazeCoords
  ) where

import Control.Monad (filterM, when)
import Control.Monad.Extra (allM)
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
  deriving Show

-- | Create a fresh cell with both right and down closed.
newCell :: Cell
newCell = Cell False False

-- | The location of a cell within a maze is just a pair @(row, col)@ (@0@-indexed).
data Coord = C { coordRow :: Word32
               , coordCol :: Word32
               }
  deriving (Show, Eq, Ord, Ix)

-- | Unique identifier for an edge, or wall, in a maze.
data EdgeId = EdgeRight Coord
            | EdgeDown Coord
  deriving (Show, Eq, Ord)

-- | Get the coordinate of an edge id.
edgeIdCoord :: EdgeId -> Coord
edgeIdCoord (EdgeRight pos) = pos
edgeIdCoord (EdgeDown  pos) = pos

-- | Get the direction of an edge id.
edgeIdDirection :: EdgeId -> Direction
edgeIdDirection (EdgeRight _) = DRight
edgeIdDirection (EdgeDown  _) = DDown

-- | Get the neighbors on either side of an edge id. (Doesn't check bounds, so
-- use in conjunction with 'stMazeInBounds'.)
edgeIdNeighbors :: EdgeId -> (Coord, Coord)
edgeIdNeighbors e = (pos, neighborCoord dir pos)
  where pos = edgeIdCoord e
        dir = edgeIdDirection e

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
neighborCoord dir (C r c) = case dir of
  DUp    -> C (r-1) c
  DDown  -> C (r+1) c
  DLeft  -> C r (c-1)
  DRight -> C r (c+1)

-- | Get the edge identifier of a cell in a particular direction. Since we don't
-- check bounds, this can return an edge outside the maze (including with a
-- negative coordinate), so always use in conjunction with 'stMazeInBounds'.
neighborEdgeId :: Direction -> Coord -> EdgeId
neighborEdgeId dir (C r c) = case dir of
  DUp -> EdgeDown (C (r-1) c)
  DDown -> EdgeDown (C r c)
  DLeft -> EdgeRight (C r (c-1))
  DRight -> EdgeRight (C r c)

-- | Mutable maze in 'ST' monad.
newtype STMaze s = STMaze { stMazeArray :: STArray s Coord Cell }

-- | Construct a new 'STMaze' with a given number of rows and columns. Both rows
-- and columns must be positive, or this function will throw an error.
newSTMaze :: Word32 -> Word32 -> ST s (STMaze s)
newSTMaze 0 _ = error "newSTMaze called with 0 rows"
newSTMaze _ 0 = error "newSTMaze called with 0 columns"
newSTMaze rows cols = STMaze <$> newArray (C 0 0, C (rows-1) (cols-1)) newCell

-- | Get the number of (rows, columns) in an 'STMaze'.
stMazeDims :: STMaze s -> ST s (Word32, Word32)
stMazeDims maze = do
  (C _ _, C hiR hiC) <- getBounds (stMazeArray maze)
  return (hiR+1, hiC+1)

-- | Get the bounds of an 'STMaze' (top-left and bottom-right corners).
stMazeBounds :: STMaze s -> ST s (Coord, Coord)
stMazeBounds = getBounds . stMazeArray

-- | Determine if a coordinate lies within an 'STMaze'\'s bounds.
stMazeInBounds :: STMaze s -> Coord -> ST s Bool
stMazeInBounds maze pos = do
  bounds <- stMazeBounds maze
  return $ inRange bounds pos

-- | Get a list of all inner edges in an 'STMaze', with neighbors on each side.
stMazeInnerEdges :: STMaze s -> ST s [EdgeId]
stMazeInnerEdges maze = do
  (numRows, numCols) <- stMazeDims maze
  return $
    [ EdgeRight (C (fromInteger r) (fromInteger c))
    | r <- [0..toInteger numRows-1], c <- [0..toInteger numCols-2] ] ++
    [ EdgeDown (C (fromInteger r) (fromInteger c))
    | r <- [0..toInteger numRows-2], c <- [0..toInteger numCols-1] ]

-- | Get the neighbor coordinate in a particular direction of an 'STMaze', if
-- one exists. If it doesn't, return 'Nothing'.
stMazeNeighborCoord :: STMaze s -> Direction -> Coord -> ST s (Maybe Coord)
stMazeNeighborCoord maze dir pos = do
  let nPos = neighborCoord dir pos
  inB <- stMazeInBounds maze (neighborCoord dir pos)
  if inB then return (Just nPos) else return Nothing

-- | Get all the neighbors of a particular cell in an 'STMaze', along with their
-- directions relative to the input cell. It doesn't matter whether there is a
-- wall between the cells.
stMazeNeighborCoords :: STMaze s -> Coord -> ST s [(Direction, Coord)]
stMazeNeighborCoords maze pos =
  let ns = [ (dir, neighborCoord dir pos) | dir <- [DUp, DDown, DLeft, DRight] ]
  in filterM (stMazeInBounds maze . snd) ns

-- | Open up one of the walls surrounding a cell, given the cell coordinate and
-- the direction of the wall relative to that coordinate. If the direction leads
-- us to a cell outside the maze, do nothing, but return 'False'.
stMazeOpen :: STMaze s -> Coord -> Direction -> ST s Bool
stMazeOpen maze pos dir = do
  let nPos = neighborCoord dir pos
  inBounds <- stMazeInBounds maze nPos
  when inBounds $ do
    let arr = stMazeArray maze
    cell <- readArray arr pos
    nCell <- readArray arr pos
    case dir of
      DUp    -> writeArray arr nPos (nCell { cellOpenDown  = True })
      DDown  -> writeArray arr pos  (cell  { cellOpenDown  = True })
      DLeft  -> writeArray arr nPos (nCell { cellOpenRight = True })
      DRight -> writeArray arr pos  (cell  { cellOpenRight = True })
  return inBounds

-- | Open up one of the walls surrounding a cell, given the cell coordinate and
-- the direction of the wall relative to that coordinate. If the direction leads
-- us to a cell outside the maze, do nothing, but return 'False'.
stMazeOpenEdge :: STMaze s -> EdgeId -> ST s Bool
stMazeOpenEdge maze e = stMazeOpen maze (edgeIdCoord e) (edgeIdDirection e)

-- | Immutable maze.
newtype IMaze = IMaze { iMazeArray :: Array Coord Cell }

-- | Freeze a mutable 'STMaze' to an immutable 'IMaze'.
freezeSTMaze :: STMaze s -> ST s IMaze
freezeSTMaze (STMaze a) = IMaze <$> freeze a

-- | Get the number of (rows, columns in an 'IMaze'.
iMazeDims :: IMaze -> (Word32, Word32)
iMazeDims maze = let (C _ _, C hiR hiC) = bounds (iMazeArray maze)
                 in (hiR+1, hiC+1)

-- | Get the bounds of an 'IMaze' (top-left and bottom-right corners).
iMazeBounds :: IMaze -> (Coord, Coord)
iMazeBounds = bounds . iMazeArray

-- | Determine if a coordinate lies within an 'IMaze'\'s bounds.
iMazeInBounds :: IMaze -> Coord -> Bool
iMazeInBounds = inRange . bounds . iMazeArray

-- | Get the cell at a given coordinate of an 'IMaze'. Does not do bounds
-- checking, so this can raise an error.
iMazeGetCell :: IMaze -> Coord -> Cell
iMazeGetCell maze pos = iMazeArray maze ! pos

-- | Given a maze, a coordinate, and a direction we'd like to move, return the
-- coordinate we are trying to move to, if it is possible to do so; otherwise
-- return 'Nothing'.
iMazeMove :: IMaze -> Coord -> Direction -> Maybe Coord
iMazeMove maze pos dir
  | nPos <- neighborCoord dir pos
  , iMazeInBounds maze nPos =
    let cell = iMazeGetCell maze pos
        nCell = iMazeGetCell maze nPos
        open = case dir of DUp -> cellOpenDown nCell
                           DDown -> cellOpenDown cell
                           DLeft -> cellOpenRight nCell
                           DRight -> cellOpenRight cell
    in if open then Just nPos else Nothing
iMazeMove _ _ _ = Nothing

-- | Extract a list of lists of coordinates from an 'IMaze', in row-major order.
iMazeCoords :: IMaze -> [[Coord]]
iMazeCoords maze = rows
  where (numRows, numCols) = iMazeDims maze
        rows = [ [ C (fromInteger r) (fromInteger c)
                 | c <- [0..toInteger numCols-1] ]
               | r <- [0..toInteger numRows-1] ]
