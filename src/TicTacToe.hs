module TicTacToe where

import Control.Monad.State
import Data.Foldable (for_)
import Data.List (intersperse, uncons)
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (for)
import qualified Data.Vector as V
import System.Random (randomRIO)

import Grid

data Cell where
  Empty :: Cell
  XCell :: Cell
  OCell :: Cell

type Board = Grid Cell

gameBoardSize :: Board -> Int
gameBoardSize board = V.length board

-- Function to create an empty board of given size
createEmptyBoard :: Size -> Board
createEmptyBoard (Size s) = generate (Size s) (const Empty)

renderCell :: Cell -> String
renderCell Empty = " "
renderCell XCell = "X"
renderCell OCell = "O"

renderBoard :: Board -> String
renderBoard board =
  unlines $ Prelude.map (concatMap renderCell) (Grid.toLists board)

printBoard :: Board -> IO ()
printBoard board = putStrLn $ renderBoard board

-- winning indices for board
getWinningIndices :: Board -> [[Index]]
getWinningIndices board = winningIndices (gameBoardSize board)
