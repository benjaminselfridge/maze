{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Maze.UI where

import Maze.Algorithms
import Maze.Core

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.Edit as B
import qualified Brick.Forms as B
import Control.Monad ((<=<))
import Data.Array.IArray
import qualified Graphics.Vty as V
import Lens.Micro.Platform
import Data.Time.Clock
import System.Random
import Data.Word
import qualified Data.Text as T
import Text.Read (readMaybe)
import qualified Data.Set as Set

maxRows :: Word32
maxRows = 20

maxCols :: Word32
maxCols = 40

data Name = NGFNumRows
          | NGFNumCols
          | NGFRecursiveBacktracking
          | NGFBinaryTree
          | NGFKruskal
          | NGFBig
          | NGFSmall
  deriving (Show, Eq, Ord)

-- | The only additional event we use is a timer event from the outside world
-- telling us the current time so we can update the 'GameState'. It doesn't
-- matter how often these ticks are received, as long as they are requently
-- enough that we can know how many seconds has passed (so something like every
-- tenth of a second should be sufficient).
data MazeEvent = Tick UTCTime

data Dialog = NoDialog
            | NewGameDialog

data SolvingState = InProgress
                  | Solved Int

data GameMode = GameMode { _gmSolvingState :: SolvingState
                         , _gmDialog :: Dialog
                         }
makeLenses ''GameMode

data Algorithm = RecursiveBacktracking
               | BinaryTree
               | Kruskal
  deriving (Show, Eq, Ord)

data Size = Big | Small
  deriving (Show, Eq, Ord)

data NewGameFormState = NewGameFormState
  { _ngfNumRows   :: Word32
  , _ngfNumCols   :: Word32
  , _ngfAlgorithm :: Algorithm
  , _ngfSize      :: Size
  }
makeLenses ''NewGameFormState

newGameForm :: NewGameFormState -> B.Form NewGameFormState e Name
newGameForm = B.newForm
  [ label "# rows (<=20): " B.@@= editShowableFieldWithValidate ngfNumRows NGFNumRows validRow
  , label "# cols (<=40): " B.@@= editShowableFieldWithValidate ngfNumCols NGFNumCols validCol
  , label "# algorithm: " B.@@= B.radioField ngfAlgorithm
    [ (RecursiveBacktracking, NGFRecursiveBacktracking, "recursive backtracking")
    , (BinaryTree, NGFBinaryTree, "binary tree")
    , (Kruskal, NGFKruskal, "kruskal's algorithm")
    ]
  , label "size: " B.@@= B.radioField ngfSize
    [ (Big, NGFBig, "big")
    , (Small, NGFSmall, "small")
    ]
  ]
  where label s w = B.padBottom (B.Pad 1) $
          (B.vLimit 1 $ B.hLimit 15 $ B.str s B.<+> B.fill ' ') B.<+> w
        validRow r | 1 <= r, r <= maxRows = Just r
                   | otherwise = Nothing
        validCol c | 1 <= c, c <= maxCols = Just c
                   | otherwise = Nothing

-- | This will be merged into @brick@, so we can remove it at some point
editShowableFieldWithValidate :: (Ord n, Show n, Read a, Show a)
                   => Lens' s a
                   -- ^ The state lens for this value.
                   -> n
                   -- ^ The resource name for the input field.
                   -> (a -> Maybe a)
                   -- ^ additional validation step for input.
                   -> s
                   -- ^ The initial form state.
                   -> B.FormFieldState s e n
editShowableFieldWithValidate stLens n validate =
    let ini = T.pack . show
        val = validate <=< (readMaybe . T.unpack . T.intercalate "\n")
        limit = Just 1
        renderText = B.txt . T.unlines
    in B.editField stLens n limit ini val renderText id

data GameState = GameState
  { _gsMaze :: IMaze
  , _gsPos :: Coord
  , _gsGen :: StdGen
  , _gsNewGameForm :: B.Form NewGameFormState MazeEvent Name
  , _gsGameMode :: GameMode
  , _gsVisitedCoords :: Set.Set Coord
  , _gsStartTime :: UTCTime
    -- ^ Time when the current game was started
  , _gsCurrentTime :: UTCTime
    -- ^ Time right now
  }

makeLenses ''GameState

gameState :: StdGen
          -> Word32
          -> Word32
          -> Algorithm
          -> Size
          -> UTCTime
          -> UTCTime
          -> GameState
gameState g numRows numCols alg size startTime currentTime =
  let (maze, g') = case alg of
        RecursiveBacktracking -> recursiveBacktracking g numRows numCols
        BinaryTree            -> binaryTree g numRows numCols
        Kruskal               -> kruskal g numRows numCols
      ngf = newGameForm (NewGameFormState numRows numCols alg size)
  in GameState maze (0, 0) g' ngf (GameMode InProgress NoDialog) Set.empty startTime currentTime

gsNewGame :: GameState -> GameState
gsNewGame gs = gameState g numRows numCols alg size st ct
  where g = gs ^. gsGen
        numRows = B.formState (gs ^. gsNewGameForm) ^. ngfNumRows
        numCols = B.formState (gs ^. gsNewGameForm) ^. ngfNumCols
        alg = B.formState (gs ^. gsNewGameForm) ^. ngfAlgorithm
        size = B.formState (gs ^. gsNewGameForm) ^. ngfSize
        st = gs ^. gsCurrentTime
        ct = gs ^. gsCurrentTime

mazeApp :: B.App GameState MazeEvent Name
mazeApp = B.App
  { B.appDraw = draw
  , B.appChooseCursor = \_ _ -> Nothing
  , B.appHandleEvent = handleEvent
  , B.appStartEvent = startEvent
  , B.appAttrMap = attrMap
  }

mazeHeight :: IMaze -> Int
mazeHeight maze = let (numRows, _) = iMazeDims maze
                  in fromIntegral (numRows * 2 + 2)

draw :: GameState -> [B.Widget Name]
draw gs = case gs ^. gsGameMode ^. gmDialog of
  NoDialog -> [ drawMain gs ]
  NewGameDialog -> [ drawNewGame gs ]

drawNewGame :: GameState -> B.Widget Name
drawNewGame gs = B.center $ B.vLimit 20 $ B.hLimit 50 $
  B.vBox [ B.renderForm (gs ^. gsNewGameForm)
         , B.center $ B.str "(press enter to start new game, esc to cancel)"
         ]

drawMain :: GameState -> B.Widget n
drawMain gs = B.vBox
  [ B.vLimit 5 $ B.center $ B.str "MAZE"
  , B.center $ B.vBox
    [ B.hCenter $ drawMaze gs
    , B.hCenter $ status (gs ^. gsGameMode ^. gmSolvingState) (secondsElapsed gs)
    ]
  , B.vLimit 5 $ B.center help
  ]

drawMaze :: GameState -> B.Widget n
drawMaze gs = B.vBox $
  (B.hBox . fmap (drawCell gs)) topRow :
  fmap (B.hBox . fmap (drawCell gs)) rows
  where (topRow:rows) = iMazeToList (gs ^. gsMaze)
        drawCell = case B.formState (gs ^. gsNewGameForm) ^. ngfSize of
          Big -> drawCellBig
          Small -> drawCellSmall

drawCellSmall :: GameState -> (Coord, Cell) -> B.Widget n
drawCellSmall gs ((r, c), cell) = B.vBox
  [ B.str $ tS
  , B.hBox [B.str lS, B.withAttr attr (B.str [dC]), B.str [rC]]
  ]
  where pos = gs ^. gsPos
        dC = if cellOpenDown  cell then ' ' else '_'
        rC = if cellOpenRight cell then ' ' else '|'
        lS = if c == 0 then "|" else ""
        tS = if r == 0
             then if c == 0 then " _ \n" else "_ \n"
             else ""
        isFinish = (r, c) == (numRows-1, numCols-1)
        isSolved = pos == (numRows-1, numCols-1)
        attr = if pos == (r, c)
               then if isFinish
                    then "solved"
                    else "pos"
               else if isFinish
                    then "finish"
                    else "blank"
        (numRows, numCols) = iMazeDims (gs ^. gsMaze)

drawCellBig :: GameState -> (Coord, Cell) -> B.Widget n
drawCellBig gs ((r, c), cell) = B.vBox
  [ B.str $ topLeftBorder ++ topBorder
  , B.hBox
    [ B.str leftBorder
    , B.str " "
    , B.withAttr attr $ B.str [mid]
    , B.str " "
    , B.str [right]
    ]
  , B.hBox
    [ B.str leftBorder
    , B.str [down , down , down]
    , B.str [bottomRight]
    ]
  ]
  where maze = gs ^. gsMaze
        pos = gs ^. gsPos
        mid = if (r, c) == pos then '*' else ' '
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
          _ -> "blank"
        (numRows, numCols) = iMazeDims maze

secondsElapsed :: GameState -> Int
secondsElapsed gs = floor $ nominalDiffTimeToSeconds $
  diffUTCTime (gs ^. gsCurrentTime) (gs ^. gsStartTime)

status :: SolvingState -> Int -> B.Widget n
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
        gs2 = gs1 & gsGameMode . gmSolvingState .~ case isSolved gs1 of
          True -> Solved (secondsElapsed gs0)
          False -> InProgress
    in gs2
  | otherwise = gs0

handleEvent :: GameState
            -> B.BrickEvent Name MazeEvent
            -> B.EventM Name (B.Next GameState)
handleEvent gs be = case gs ^. gsGameMode ^. gmDialog of
  NoDialog -> case gs ^. gsGameMode . gmSolvingState of
    InProgress -> case be of
      B.VtyEvent (V.EvKey (V.KChar 'q') []) -> B.halt gs
      B.VtyEvent (V.EvKey (V.KChar 'n') []) ->
        B.continue (gs & gsGameMode . gmDialog .~ NewGameDialog)
      B.VtyEvent (V.EvKey V.KUp []) -> B.continue (gsMove gs DUp)
      B.VtyEvent (V.EvKey V.KDown []) -> B.continue (gsMove gs DDown)
      B.VtyEvent (V.EvKey V.KLeft []) -> B.continue (gsMove gs DLeft)
      B.VtyEvent (V.EvKey V.KRight []) -> B.continue (gsMove gs DRight)
      B.AppEvent (Tick currentTime) -> B.continue (gs & gsCurrentTime .~ currentTime)
      _ -> B.continue gs
    Solved _ -> case be of
      B.VtyEvent (V.EvKey (V.KChar 'q') []) -> B.halt gs
      B.VtyEvent (V.EvKey (V.KChar 'n') []) ->
        B.continue (gs & gsGameMode . gmDialog .~ NewGameDialog)
      B.AppEvent (Tick currentTime) -> B.continue (gs & gsCurrentTime .~ currentTime)
      _ -> B.continue gs
  NewGameDialog -> case be of
    B.VtyEvent (V.EvKey V.KEnter []) ->
      B.continue (gsNewGame gs)
    B.VtyEvent (V.EvKey V.KEsc []) ->
      B.continue (gs & gsGameMode . gmDialog .~ NoDialog)
    _ -> do f' <- B.handleFormEvent be (gs ^. gsNewGameForm)
            B.continue (gs & gsNewGameForm .~ f')

startEvent :: GameState -> B.EventM Name GameState
startEvent gs = return gs

attrMap :: GameState -> B.AttrMap
attrMap _ = B.attrMap V.defAttr
  [ ("start", V.defAttr)
  , ("finish", V.withBackColor V.defAttr V.red)
  , ("solved", V.withBackColor V.defAttr V.green)
  , ("blank", V.defAttr)
  , ("pos", V.withBackColor V.defAttr V.blue)
  , (B.formAttr, V.defAttr)
  , (B.editAttr, V.white `B.on` V.black)
  , (B.editFocusedAttr, V.black `B.on` V.yellow)
  , (B.focusedFormInputAttr, V.black `B.on` V.yellow)
  , (B.invalidFormInputAttr, V.white `B.on` V.red)
  ]
