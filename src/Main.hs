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
import Brick.AttrMap (attrMap)
import Graphics.Vty.Attributes (defAttr)

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
  deriving (Show)
      
cellWidget :: Bool -> Index -> Cell -> Widget Void
cellWidget selected _ cell =
  let 
    cellText = renderCell cell
    baseWidget = 
      withAttr (attrName cellText) $
        Brick.str cellText
  in
    if selected then
      border baseWidget
    else
      padAll 1 baseWidget

boardWidget :: Board -> Index -> Widget Void
boardWidget board focusIdx =
  let
    renderCell :: Index -> Cell -> Widget Void
    renderCell idx cell =
      let
        selected = idx == focusIdx
      in
        cellWidget selected idx cell
    tableRows = V.toList $ V.imap (\y row -> V.toList $ V.imap (\x cell -> renderCell (Index y x) cell) row) board
  in
    renderTable $ table tableRows

handleEvent ::
  Board ->
  Player ->
  BrickEvent Void Void ->
  EventM Void AppState ()
handleEvent board player event =
  case event of
    VtyEvent (EvKey key []) ->
      case key of
        KLeft  -> modify $ \st -> st { focus = wraparound (gameBoardSize board) (up (st.focus)) }
        KRight -> modify $ \st -> st { focus = wraparound (gameBoardSize board) (down (st.focus)) }
        KUp    -> modify $ \st -> st { focus = wraparound (gameBoardSize board) (left (st.focus)) }
        KDown  -> modify $ \st -> st { focus = wraparound (gameBoardSize board) (right (st.focus)) }
        KEsc   -> halt
        _      -> pure ()
    _ -> pure ()

gameAttrMap :: AttrMap
gameAttrMap =
  attrMap
    (brightWhite `on` black)
    [ (attrName "1", fg brightBlue)
    , (attrName "2", fg green)
    , (attrName "3", fg brightRed)
    , (attrName "4", fg blue)
    , (attrName "5", fg red)
    , (attrName "6", fg cyan)
    , (attrName "7", fg brightBlack)
    , (attrName "8", fg white)
    ]

app :: Board -> Cell -> Player -> App AppState Void Void
app board cell player = 
  App 
    { appDraw = \st -> [boardWidget (appBoard st) (focus st)]
    , appChooseCursor = \_ _ -> Nothing
    , appHandleEvent = handleEvent board player
    , appStartEvent = pure ()
    , appAttrMap = \_ -> gameAttrMap
}

initialAppState :: Options -> AppState
initialAppState opts =
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
  let startGame = initialAppState opts
  void $ defaultMain (app (appBoard startGame) Empty (currentPlayer startGame)) startGame
