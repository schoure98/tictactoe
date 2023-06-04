module TicTacToe where

import Control.Monad.State
import Data.Foldable (for_)
import Data.List (uncons, intersperse)
import Data.List qualified as List
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable (for)
import Data.Vector qualified as V
import System.Random (randomRIO)

import Grid

data Cell where
  Empty :: Cell
  XCell :: Cell
  OCell :: Cell
  deriving (Show, Eq)

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

-- gameWon :: Player -> getWinningIndices -> Bool
-- gameWon player board = Grid.any (getWinningIndices (gameBoardSize board))


-- gameWon :: Board -> Player -> Bool
-- gameWon board player = Grid.any (indicesCoveredByPlayer player) (getWinningIndices board)
--   where
--     indicesCoveredByPlayer :: Player -> [[Index]] -> Bool
--     indicesCoveredByPlayer player indices = Grid.any (Grid.all (\index -> cellAt board index == Just player)) indices
    
--     cellAt :: Board -> Index -> Maybe Cell
--     cellAt board index = index board >>= index >>= indexToCell
--     where
--       indexToCell :: Cell -> Maybe Cell
--       indexToCell Empty = Nothing
--       indexToCell XCell = Just XCell
--       indexToCell OCell = Just OCell


