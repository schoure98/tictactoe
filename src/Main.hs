module Main where

import Brick
import Brick.Widgets.Border (border)
import Brick.Widgets.Table 
import Brick.Widgets.Center (center)
import Brick.Types (Widget, BrickEvent(..))
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Vector qualified as V
import Graphics.Vty qualified as Vty
import Graphics.Vty.Attributes.Color
import Graphics.Vty.Input.Events (Event(..), Key(..), Modifier(..), Button(..))
import Options.Applicative
import System.Exit
import Data.Void
import Data.List (intersperse)

import Grid
import TicTacToe

data AppState = AppState
  { appBoard :: Board
  , currentPlayer :: Player
  , gameStatus :: GameStatus
  , focus :: Index
  } deriving (Show)

data GameStatus where 
  GameInProgress :: GameStatus
  GameWon :: GameStatus
  GameDraw :: GameStatus
  GameOver :: GameStatus
  deriving (Show, Eq)

cellWidget :: Cell -> Widget Void
cellWidget Empty = padAll 1 $ Brick.str " "
cellWidget XCell = padAll 1 $ Brick.str "X"
cellWidget OCell = padAll 1 $ Brick.str "O"

boardWidget :: Board -> Widget Void
boardWidget board = renderTable $ table rows
  where
    rows = V.toList $ V.map (V.toList . V.map cellWidget) board

-- handleEvent :: Board -> BrickEvent Void Void -> EventM Void AppState ()
-- handleEvent _ event =
--   case event of
--     VtyEvent (EvKey key []) ->
--       case key of
--         KEsc -> halt
--         _    -> pure ()
--     _ -> pure ()


-- handleEvent :: AppState -> BrickEvent Void Void -> EventM Void (Next AppState)
-- handleEvent st (VtyEvent (EvKey key [])) =
--   case key of
--     KLeft -> continue $ moveCursor st MoveLeft
--     KRight -> continue $ moveCursor st MoveRight
--     KUp -> continue $ moveCursor st MoveUp
--     KDown -> continue $ moveCursor st MoveDown
--     KEsc -> halt st
--     KEnter -> continue $ updateBoard st
--     _ -> continue st
-- handleEvent st _ = continue st


-- app :: Options -> App AppState () ()
-- app opts =
--   App
--     { appDraw = \st -> [boardWidget  st]
--     , appChooseCursor = \_ _ -> Nothing
--     , appHandleEvent = handleEvent
--     , appStartEvent = pure ()
--     , appAttrMap = \_ -> attrMap Vty.defAttr []
--     }



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

main :: IO ()
main = do
  opts <- options
  let size = gridSize (opts.boardSize)
  let emptyBoard = createEmptyBoard (Size size)
  let widget = center $ boardWidget emptyBoard
  simpleMain widget