{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Maze.UI where

import Maze.Algorithms
import Maze.Types

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as B
import Data.Array.IArray
import qualified Graphics.Vty as V
import Lens.Micro.Platform
import Data.Time.Clock
import System.Random
import Data.Word

type Resource = ()

-- | The only additional event we use is a timer event from the outside world
-- telling us the current time so we can update the 'GameState'. It doesn't
-- matter how often these ticks are received, as long as they are requently
-- enough that we can know how many seconds has passed (so something like every
-- tenth of a second should be sufficient).
data MazeEvent = Tick UTCTime

data GameState = GameState { _gsMaze :: IMaze
                           , _gsPos :: Coord
                           , _gsGen :: StdGen
                           , _gsGameMode :: GameMode
                           , _gsStartTime :: UTCTime
                             -- ^ Time when the current game was started
                           , _gsCurrentTime :: UTCTime
                             -- ^ Time right now
                           }

data GameMode = InProgress
              -- | The 'Int' is the number of seconds it took to solve.
              | Solved Int

makeLenses ''GameState

newGameState :: StdGen -> Word32 -> Word32 -> UTCTime -> UTCTime -> GameState
newGameState g rows cols startTime currentTime =
  let (maze, g') = recursiveBacktracking g rows cols
  in GameState maze (0, 0) g' InProgress startTime currentTime

gsNewGame :: GameState -> GameState
gsNewGame gs = newGameState (gs ^. gsGen) numRows numCols (gs^.gsCurrentTime) (gs^.gsCurrentTime)
  where (numRows, numCols) = iMazeDims (gs ^. gsMaze)

mazeApp :: B.App GameState MazeEvent Resource
mazeApp = B.App
  { B.appDraw = draw
  , B.appChooseCursor = (\_ _ -> Nothing)
  , B.appHandleEvent = handleEvent
  , B.appStartEvent = startEvent
  , B.appAttrMap = attrMap
  }

mazeHeight :: IMaze -> Int
mazeHeight maze = let (numRows, _) = iMazeDims maze
                  in fromIntegral (numRows * 2 + 2)

draw :: GameState -> [B.Widget n]
draw gs = [ B.vBox
            [ B.vLimit 5 (B.center (B.str "MAZE"))
            , B.vBox
              [ B.vLimit (mazeHeight (gs ^. gsMaze))
                (B.center (drawMaze (gs ^. gsMaze) (gs ^. gsPos)))
              , B.vLimit 1 $ B.center $ status (gs ^. gsGameMode) (secondsElapsed gs)
              ]
            , B.center help
            ]
          ]

drawMaze :: IMaze -> Coord -> B.Widget n
drawMaze maze pos = B.vBox $
  (B.hBox . fmap (drawCell maze pos)) topRow :
  fmap (B.hBox . fmap (drawCell maze pos)) rows
  where (topRow:rows) = iMazeToList maze

drawCell :: IMaze -> Coord -> (Coord, Cell) -> B.Widget n
drawCell maze pos ((r, c), cell) = B.vBox
  [ B.str $ topLeftBorder ++ topBorder
  , B.hBox
    [ B.str leftBorder
    , B.withAttr attr $ B.str " "
    , B.withAttr attr $ B.str [mid]
    , B.withAttr attr $ B.str " "
    , B.str [right]
    ]
  , B.hBox
    [ B.str leftBorder
    , B.withAttr attr $ B.str [down , down , down]
    , B.str [bottomRight]
    ]
  ]
  where mid = if (r, c) == pos then '*' else ' '
        down  = if cellOpenDown  cell then ' ' else '_'
        right = if cellOpenRight cell then ' ' else '|'
        bottomRight = if cellOpenRight cell then ' ' else '|'
        leftBorder = if c == 0 then "|" else ""
        topBorder = if r == 0 then "___ " else ""
        topLeftBorder = if (r == 0 && c == 0) then " " else ""
        isStart = r == 0 && c == 0
        isFinish = r == numRows-1 && c == numCols-1
        attr = case (isStart, isFinish) of
          (True, _) -> "start"
          (_, True) -> "finish"
          _ -> "cell"
        (numRows, numCols) = iMazeDims maze

secondsElapsed :: GameState -> Int
secondsElapsed gs = floor $ nominalDiffTimeToSeconds $
  diffUTCTime (gs ^. gsCurrentTime) (gs ^. gsStartTime)

status :: GameMode -> Int -> B.Widget n
status InProgress i = B.str $ "Time: " ++ show i ++ "s"
status (Solved i) _ = B.str $ "Solved (" ++ show i ++ "s)! Nice job!"

help :: B.Widget n
help = B.hBox
  [ B.padLeftRight 1 $
    B.vBox [ B.str "up/down/left/right"
           , B.str "n"
           , B.str "q"
           ]
  , B.padLeftRight 1 $
    B.vBox [ B.str "move position"
           , B.str "new game"
           , B.str "quit"
           ]
  ]

isSolved :: GameState -> Bool
isSolved gs = let (numRows, numCols) = iMazeDims (gs ^. gsMaze)
              in gs ^. gsPos == (numRows-1, numCols-1)

gsMove :: GameState -> Direction -> GameState
gsMove gs0 dir
  | iMazeCanMove (gs0 ^. gsMaze) (gs0 ^. gsPos) dir =
    let gs1 = gs0 & gsPos %~ neighborCoord dir
        gs2 = gs1 & gsGameMode .~ case isSolved gs1 of
          True -> Solved (secondsElapsed gs0)
          False -> InProgress
    in gs2
  | otherwise = gs0

handleEvent :: GameState
            -> B.BrickEvent Resource MazeEvent
            -> B.EventM Resource (B.Next GameState)
handleEvent gs be = case gs ^. gsGameMode of
  InProgress -> case be of
    B.VtyEvent (V.EvKey (V.KChar 'q') []) -> B.halt gs
    B.VtyEvent (V.EvKey (V.KChar 'n') []) -> B.continue (gsNewGame gs)
    B.VtyEvent (V.EvKey V.KUp []) -> B.continue (gsMove gs DUp)
    B.VtyEvent (V.EvKey V.KDown []) -> B.continue (gsMove gs DDown)
    B.VtyEvent (V.EvKey V.KLeft []) -> B.continue (gsMove gs DLeft)
    B.VtyEvent (V.EvKey V.KRight []) -> B.continue (gsMove gs DRight)
    B.AppEvent (Tick currentTime) -> B.continue (gs & gsCurrentTime .~ currentTime)
    _ -> B.continue gs
  Solved _ -> case be of
    B.VtyEvent (V.EvKey (V.KChar 'q') []) -> B.halt gs
    B.VtyEvent (V.EvKey (V.KChar 'n') []) -> B.continue (gsNewGame gs)
    B.AppEvent (Tick currentTime) -> B.continue (gs & gsCurrentTime .~ currentTime)
    _ -> B.continue gs


startEvent :: GameState -> B.EventM Resource GameState
startEvent gs = return gs

attrMap :: GameState -> B.AttrMap
attrMap _ = B.attrMap V.defAttr
  [ ("start", V.withBackColor V.defAttr V.green)
  , ("finish", V.withBackColor V.defAttr V.red)
  , ("cell", V.defAttr)
  ]
