module Main where

import Brick
import Brick.Widgets.Border (border)
import Brick.Widgets.Table 
import Brick.Widgets.Center (center)
import Brick.Types (Widget)

import Data.Vector qualified as V

import Control.Monad
import Data.Void
import Graphics.Vty.Attributes.Color
import Graphics.Vty.Input.Events (Event(..), Key(..), Modifier(..), Button(..))
import Options.Applicative

import Data.List (intersperse)

import Grid
import TicTacToe

-- data GameState where 
--   GameState ::
--    { board :: Board
--    , currentCover :: Player
--    } -> GameState

-- data AppState where
--   AppState ::
--     { field :: Field
--     , focus :: Index
--     , board :: Board
--     , currentCover :: Player
--     } -> AppState
--     deriving (Show)


-- A Brick UI widget to represent a single cell on the game board. Surrounds
-- the cell text with a border if it's selected, or one space of padding if
-- it's unselected.
-- cellWidget ::
--   Bool -> Index ->
--   (FieldCell, SurveyCell, Cell) ->
--   Widget Void
-- cellWidget selected i (fc, sc, cc) =
--   let
--     cellText = renderCell fc sc cc
--     baseWidget =
--       withAttr (attrName cellText) $
--         Brick.str cellText
--   in
--     if selected then
--       border baseWidget
--     else
--       padAll 1 baseWidget

cellWidget :: Cell -> Widget Void
cellWidget Empty = padAll 1 $ Brick.str " "
cellWidget XCell = padAll 1 $ Brick.str "X"
cellWidget OCell = padAll 1 $ Brick.str "O"



boardWidget :: Board -> Widget Void
boardWidget board = renderTable $ table rows
  where
    rows = V.toList $ V.map (V.toList . V.map cellWidget) board

-- boardWidget :: Board -> Widget Void
-- boardWidget board =
--   renderTable $ table $ Grid.toLists $
--     Grid.zipWith3 cellWidget
--       (Grid.map (\i -> i == st.focus) (shape board))
--       (shape board)
--       (Grid.zip3 board (surveyField field) st.cover)

-- makeMove :: AppState -> AppState
-- makeMove st =
--   let
--     currentFocus = focus st
--     currentPlayer = currentPlayer st
--     currentField = field st
--     currentCell = index currentField currentFocus
--   in
--     case currentCell of
--       Just Empty -> st -- Handle the logic of making a move here
--       _ -> st


-- -- Handle a BrickEvent, which represents a user input or some other change in
-- -- the terminal state outside our application. Brick gives us two relevant
-- -- commands in the EventM monad:
-- --   halt takes a final AppState and exits the UI thread
-- --   continue takes a next AppState and continues the UI thread
-- handleEvent ::
--   Dimensions -> Field -> Survey ->
--   BrickEvent Void Void ->
--   EventM Void AppState ()
-- handleEvent dim field survey event =
--   case event of
--     -- The VtyEvent constructor with an EvKey argument indicates that the user
--     -- has pressed a key on the keyboard. The empty list in the pattern
--     -- indicates that no modifier keys (Shift/Ctrl/...) were being held down
--     -- while the key was pressed.
--     VtyEvent (EvKey key []) ->
--       case key of
--         KLeft  -> continue $ st { focus = wraparound (size dim) (left  (focus st)) }
--         KRight -> continue $ st { focus = wraparound (size dim) (right (focus st)) }
--         KUp    -> continue $ st { focus = wraparound (size dim) (up    (focus st)) }
--         KDown  -> continue $ st { focus = wraparound (size dim) (down  (focus st)) }
--         KEsc   -> halt st
--         KEnter -> continue (makeMove st)
--         _      -> constinue st
--     -- We don't care about any other kind of events, at least in this version
--     -- of the code.
--     _ -> pure ()

data Options where
    Options :: 
        { boardSize :: Grid.Size
        , firstPlayer :: Player
        } -> Options
        deriving Show

optionsParser :: Parser Options
optionsParser = do
  size  <- option auto (short 's' <> value 3 <> help "Size of the board")
  player <- option auto (short 'p' <> value X <> help "Which player goes first, X or O")
  
  pure $ Options
    { boardSize = Size size
    , firstPlayer = player
    }

options :: IO Options
options =
  execParser $
    info
      (helper <*> optionsParser)
      (fullDesc <> progDesc "Play a game of Tic Tac Toe!")

-- initialGameState :: Options -> GameState
-- initialGameState opts =
--   GameState
--     { board = createEmptyBoard (boardSize opts)
--     , currentCover = firstPlayer opts
--     }
    

main :: IO ()
main = do
  opts <- options
  let size = gridSize (opts.boardSize)
  let emptyBoard = createEmptyBoard (Size size)
  let widget = center $ boardWidget emptyBoard
  simpleMain widget