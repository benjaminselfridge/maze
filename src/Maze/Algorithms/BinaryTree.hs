module Maze.Algorithms.BinaryTree
  ( binaryTree
  ) where

import Maze.Core

import Control.Monad ( forM_, when, void )
import Control.Monad.ST
import Data.Ix
import Data.STRef
import Data.Word
import System.Random

-- | Build a maze using the "binary tree" algorithm. For each cell, randomly
-- remove the wall above or to the left, but not both.
binaryTree :: RandomGen g => g -> Word32 -> Word32 -> (IMaze, g)
binaryTree g rows cols = runST $ do
  gRef <- newSTRef g
  maze <- newSTMaze rows cols
  coords <- range <$> stMazeBounds maze
  forM_ coords $ \pos -> do
    g <- readSTRef gRef
    let (b, g') = random g
    if b
      then do success <- stMazeOpen maze pos DUp
              when (not success) $ void $ stMazeOpen maze pos DLeft
      else do success <- stMazeOpen maze pos DLeft
              when (not success) $ void $ stMazeOpen maze pos DUp
    writeSTRef gRef g'
  imaze <- freezeSTMaze maze
  g' <- readSTRef gRef
  return (imaze, g')
