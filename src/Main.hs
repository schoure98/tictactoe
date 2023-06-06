module Main where

import Brick
import Brick.Types (BrickEvent(..), Widget)
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center)
import Brick.Widgets.Table
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.List (intersperse)
import qualified Data.Vector as V
import Data.Void
import qualified Graphics.Vty as Vty
import Graphics.Vty.Attributes.Color
import Graphics.Vty.Input.Events (Button(..), Event(..), Key(..), Modifier(..))
import Options.Applicative
import System.Exit

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

cellWidget :: Cell -> Widget Void
cellWidget Empty = padAll 1 $ Brick.str " "
cellWidget XCell = padAll 1 $ Brick.str "X"
cellWidget OCell = padAll 1 $ Brick.str "O"

boardWidget :: Board -> Widget Void
boardWidget board = renderTable $ table rows
  where
    rows = V.toList $ V.map (V.toList . V.map cellWidget) board

handleEvent :: AppState -> BrickEvent Void Void -> EventM Void AppState
handleEvent st event =
  case event of
    VtyEvent (EvKey key []) ->
      case key of
        KEsc -> halt st
        _ -> continue st
    _ -> continue st

initialState :: Options -> AppState
initialState opts =
  AppState
    { appBoard = createEmptyBoard (opts.boardSize)
    , currentPlayer = opts.firstPlayer
    , gameStatus = GameInProgress
    , focus = Index 0 0
    }

data Options where
  Options
    :: { boardSize :: Grid.Size
       , firstPlayer :: Player}
    -> Options

optionsParser :: Parser Options
optionsParser = do
  size <- option auto (short 's' <> value 3 <> help "Size of the board")
  player <-
    option auto (short 'p' <> value X <> help "Which player goes first, X or O")
  pure $ Options {boardSize = Size size, firstPlayer = player}

options :: IO Options
options =
  execParser
    $ info
        (helper <*> optionsParser)
        (fullDesc <> progDesc "Play a game of Tic Tac Toe!")

main :: IO ()
main = do
  opts <- options
  let size = gridSize (opts.boardSize)
  let emptyBoard = createEmptyBoard (Size size)
  let widget = center $ boardWidget emptyBoard
  simpleMain widget

main :: IO ()
main = do
  opts <- options
  let state = initialState opts
  endState <- defaultMain (app opts) state
  return ()
