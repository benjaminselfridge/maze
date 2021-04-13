{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- | This module stores the basic types and low-level operations for building
-- 2-dimensional rectangular mazes. We build the maze in 'ST' and then freeze it
-- when we're done, using mutable and immutable arrays.
module Maze.Core
  ( -- * Coordinates, directions, and walls
    Coord
  , coordRow
  , coordCol
  , Wall
  , wallNeighbors
  , wallDirection
  , Direction(..)
    -- * Mutable maze
  , STMaze
  , newSTMaze
  , stMazeBounds
  , stMazeInnerWalls
  , stMazeNeighborCoords
  , stMazeOpenCoordDir
  , stMazeOpenWall
    -- * Immutable maze
  , IMaze
  , freezeSTMaze
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

-- | The location of a cell within a maze.
data Coord = C { coordRow :: Word32
                 -- ^ @0@-indexed row of coordinate
               , coordCol :: Word32
                 -- ^ @0@-indexed column of coordinate
               }
  deriving (Show, Eq, Ord, Ix)

-- | Unique identifier for a wall separating two cells of a maze. Note that in
-- the context of a particular maze, a wall could be either "on" or "off" -- if
-- it is off, then a player can cross it, and if it is on, then they cannot.
data Wall = WallRight Coord
          | WallDown Coord
  deriving (Show, Eq, Ord)

-- | Get the coordinate of a wall. (internal)
wallCoord :: Wall -> Coord
wallCoord (WallRight pos) = pos
wallCoord (WallDown  pos) = pos

-- | Get the neighbors on either side of a wall.
wallNeighbors :: Wall -> (Coord, Coord)
wallNeighbors e = (pos, neighborCoord dir pos)
  where pos = wallCoord e
        dir = wallDirection e

-- | Get the direction of a wall. Either returns 'DRight' or 'DDown'. Use in
-- conjunction with @wallNeighbors@. In particular, if @wallNeighbors wall@
-- returns @(c1, c2)@ and @wallDirection wall@ returns @DRight@, that means that
-- @c2@ lies to the right of @c1@, and @wall@ is the wall that separates them.
wallDirection :: Wall -> Direction
wallDirection (WallRight _) = DRight
wallDirection (WallDown  _) = DDown

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

-- | Get the wall identifier of a cell in a particular direction. Since we don't
-- check bounds, this can return an wall outside the maze (including with a
-- negative coordinate), so always use in conjunction with 'stMazeInBounds'.
neighborWall :: Direction -> Coord -> Wall
neighborWall dir (C r c) = case dir of
  DUp    -> WallDown  (C (r-1) c)
  DDown  -> WallDown  (C r c)
  DLeft  -> WallRight (C r (c-1))
  DRight -> WallRight (C r c)

-- | Mutable maze in 'ST' monad.
newtype STMaze s = STMaze { stMazeArray :: STArray s Coord Cell }

-- | Construct a new 'STMaze' with a given number of rows and columns. Both rows
-- and columns must be positive, or this function will throw an error.
newSTMaze :: Word32 -> Word32 -> ST s (STMaze s)
newSTMaze 0 _ = error "newSTMaze called with 0 rows"
newSTMaze _ 0 = error "newSTMaze called with 0 columns"
newSTMaze rows cols = STMaze <$> newArray (C 0 0, C (rows-1) (cols-1)) newCell

-- | Get the bounds of an 'STMaze' (top-left and bottom-right corners).
stMazeBounds :: STMaze s -> ST s (Coord, Coord)
stMazeBounds = getBounds . stMazeArray

-- | Determine if a coordinate lies within an 'STMaze'\'s bounds.
stMazeInBounds :: STMaze s -> Coord -> ST s Bool
stMazeInBounds maze pos = do
  bounds <- stMazeBounds maze
  return $ inRange bounds pos

-- | Get a list of all inner walls in an 'STMaze', with neighbors on each side.
stMazeInnerWalls :: STMaze s -> ST s [Wall]
stMazeInnerWalls maze = do
  (_, (C hiR hiC)) <- stMazeBounds maze
  return $
    [ WallRight (C (fromInteger r) (fromInteger c))
    | r <- [0..toInteger hiR], c <- [0..(toInteger hiC)-1] ] ++
    [ WallDown (C (fromInteger r) (fromInteger c))
    | r <- [0..(toInteger hiR)-1], c <- [0..toInteger hiC] ]

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
stMazeOpenCoordDir :: STMaze s -> Coord -> Direction -> ST s Bool
stMazeOpenCoordDir maze pos dir = do
  let nPos = neighborCoord dir pos
  inBounds <- stMazeInBounds maze nPos
  when inBounds $ do
    let arr = stMazeArray maze
    cell <- readArray arr pos
    nCell <- readArray arr nPos
    case dir of
      DUp    -> writeArray arr nPos (nCell { cellOpenDown  = True })
      DDown  -> writeArray arr pos  (cell  { cellOpenDown  = True })
      DLeft  -> writeArray arr nPos (nCell { cellOpenRight = True })
      DRight -> writeArray arr pos  (cell  { cellOpenRight = True })
  return inBounds

-- | Open up one of the walls surrounding a cell, given the cell coordinate and
-- the direction of the wall relative to that coordinate. If the direction leads
-- us to a cell outside the maze, do nothing, but return 'False'.
stMazeOpenWall :: STMaze s -> Wall -> ST s Bool
stMazeOpenWall maze e = stMazeOpenCoordDir maze (wallCoord e) (wallDirection e)

-- | Immutable maze.
newtype IMaze = IMaze { iMazeArray :: Array Coord Cell }

-- | Freeze a mutable 'STMaze' to an immutable 'IMaze'.
freezeSTMaze :: STMaze s -> ST s IMaze
freezeSTMaze (STMaze a) = IMaze <$> freeze a

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
  where (_, (C hiR hiC)) = iMazeBounds maze
        rows = [ [ C (fromInteger r) (fromInteger c)
                 | c <- [0..toInteger hiC] ]
               | r <- [0..toInteger hiR] ]
