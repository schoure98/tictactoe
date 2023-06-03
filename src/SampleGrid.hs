module Main where

import Data.List (intersperse)

data Size = Size { size :: Int }
  deriving (Eq, Ord, Show, Read)

data Cell = Empty | X | O
  deriving (Eq, Show)

type Board = [[Cell]]

-- Function to create an empty board of given size
createEmptyBoard :: Size -> Board
createEmptyBoard (Size s) = replicate s (replicate s Empty)

-- Function to display the board
displayBoard :: Board -> String
displayBoard board = unlines (map displayRow board)
  where
    displayRow :: [Cell] -> String
    displayRow = concat . intersperse "|" . map displayCell

    displayCell :: Cell -> String
    displayCell Empty = " "
    displayCell X = "X"
    displayCell O = "O"

-- Example usage
main :: IO ()
main = do
  let size = Size 3 -- Set the size of the board
  let emptyBoard = createEmptyBoard size
  putStrLn (displayBoard emptyBoard)
