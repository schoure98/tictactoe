module TicTacToe where

import Control.Monad.State
import Data.Foldable (for_)
import Data.List (uncons, intersperse)
import Data.List qualified as List
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable (for)
import Data.Vector qualified as Vector
import System.Random (randomRIO)

import Grid

data Cell where
  Empty :: Cell
  XCell :: Cell
  OCell :: Cell
  deriving (Show, Eq)

type Board = Grid Cell

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
