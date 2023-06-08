module Main where

import Brick
import Brick.AttrMap (attrMap)
import Brick.Types (BrickEvent(..), Widget)
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center)
import Brick.Widgets.Table
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.List (intersperse)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Data.Void
import qualified Graphics.Vty as Vty
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
  , markedCells :: Set Index
  } deriving (Show)

data GameStatus where
  GameInProgress :: GameStatus
  GameWon :: GameStatus
  GameDraw :: GameStatus
  GameOver :: GameStatus
  deriving (Show, Eq, Ord, Read)

cellWidget :: Bool -> Index -> Cell -> Widget Void
cellWidget selected _ cell =
  let cellText = renderCell cell
      baseWidget = withAttr (attrName cellText) $ Brick.str cellText
   in if selected
        then border baseWidget
        else padAll 1 baseWidget

boardWidget :: Board -> Index -> Widget Void
boardWidget board focusIdx =
  let printCellValue :: Index -> Cell -> Widget Void
      printCellValue idx cell =
        let selected = idx == focusIdx
         in cellWidget selected idx cell
      tableRows =
        V.toList
          $ V.imap
              (\y row ->
                 V.toList $ V.imap (\x cell -> printCellValue (Index x y) cell) row)
              board
   in renderTable $ table tableRows

selectCell :: Cell -> Player -> AppState -> Maybe AppState
selectCell cell player st
  | cell == Empty = Just (updateBoard (st.focus) st)
  | otherwise = Nothing
  where
    updateBoard idx st' = st' { appBoard = replace idx (playerToCell player) (st'.appBoard), currentPlayer = switchPlayer player }



selectCellM :: EventM Void AppState ()
selectCellM = do
  st <- get
  let selectedCell = index (st.appBoard) (st.focus)
  case selectedCell of
    Just cell -> case selectCell cell (st.currentPlayer) st of
      Just updatedState -> put updatedState
      Nothing -> pure ()
    Nothing -> pure ()

handleEvent ::
  BrickEvent Void Void ->
  EventM Void AppState ()
handleEvent event =
  case event of
    VtyEvent (EvKey key []) -> do
      st <- get
      case key of
        KLeft  -> modify $ \st' -> st' { focus = (wraparound (gameBoardSize (st.appBoard)) (left (st.focus))) }
        KRight -> modify $ \st' -> st' { focus = (wraparound (gameBoardSize (st.appBoard)) (right (st.focus))) }
        KUp    -> modify $ \st' -> st' { focus = (wraparound (gameBoardSize (st.appBoard)) (up (st.focus))) }
        KDown  -> modify $ \st' -> st' { focus = (wraparound (gameBoardSize (st.appBoard)) (down (st.focus))) }
        KEnter -> selectCellM 
        KEsc   -> halt
        _      -> pure ()
    _ -> pure ()

gameAttrMap :: AttrMap
gameAttrMap =
  attrMap
    (brightWhite `on` black)
    [ (attrName (renderCell XCell), fg brightBlue)
    , (attrName (renderCell OCell), fg green)
    ]



app :: Board -> Cell -> Player -> App AppState Void Void
app board cell player =
  App
    { appDraw = \st -> [boardWidget (st.appBoard) (st.focus)]
    , appChooseCursor = \_ _ -> Nothing
    , appHandleEvent = handleEvent
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
    , markedCells = Set.empty
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
  let (AppState startBoard startPlayer _ _ _) = startGame
  finalState <- defaultMain (app startBoard Empty startPlayer) startGame

  let (AppState endBoard endPlayer _ _ _) = finalState

  if gameWon endBoard endPlayer
    then putStrLn $ renderCell (playerToCell endPlayer) ++ " wins!"
    else if gameLost endBoard endPlayer
           then putStrLn $ renderCell (playerToCell (switchPlayer endPlayer)) ++ " wins!"
           else if gameDraw endBoard
                  then putStrLn "It's a tie game!"
                  else pure ()
  printBoard endBoard
