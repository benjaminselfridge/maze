module Main where

import UI

import Control.Monad (void, forever)

import Brick
import Brick.BChan
import Data.Time.Clock
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
    (gameState g 10 10 RecursiveBacktracking Big st st)
