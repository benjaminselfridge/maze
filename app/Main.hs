module Main where

import Control.Monad (void, forever)

import Brick
import Brick.BChan
import Data.Time.Clock
import Maze.UI
import Graphics.Vty
import Control.Concurrent
import System.Random

main :: IO ()
main = do
  let builder = mkVty defaultConfig
  initialVty <- builder
  eventChannel <- newBChan 10
  void . forkIO $ forever $ do
    t <- getCurrentTime
    writeBChan eventChannel (Tick t)
    threadDelay 100000

  st <- getCurrentTime
  g <- getStdGen
  void $ customMain
    initialVty
    builder
    (Just eventChannel)
    mazeApp
    (newGameState g 10 10 st st)
