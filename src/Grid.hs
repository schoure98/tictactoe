module Grid where

import qualified Data.List as List
import Data.Maybe
import Data.Vector (Vector, (!?))
import qualified Data.Vector as Vector

type Grid a = Vector (Vector a)

toLists :: Grid a -> [[a]]
toLists = List.map Vector.toList . Vector.toList

toList :: Grid a -> [a]
toList = concat . toLists

map :: (a -> b) -> Grid a -> Grid b
map = Vector.map . Vector.map

zipWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipWith = Vector.zipWith . Vector.zipWith

zipWith3 :: (a -> b -> c -> d) -> Grid a -> Grid b -> Grid c -> Grid d
zipWith3 = Vector.zipWith3 . Vector.zipWith3

zip :: Grid a -> Grid b -> Grid (a, b)
zip = Grid.zipWith (,)

zip3 :: Grid a -> Grid b -> Grid c -> Grid (a, b, c)
zip3 = Grid.zipWith3 (,,)

foldr :: (a -> b -> b) -> b -> Grid a -> b
foldr f z = Vector.foldr (flip (Vector.foldr f)) z

data Size where
  Size :: Int -> Size

-- player
data Player
  = X
  | O
  deriving (Eq, Show, Read)

switchPlayer :: Player -> Player
switchPlayer X = O
switchPlayer O = X

data Index where
  Index
    :: { column :: Int
       , row :: Int}
    -> Index
    deriving (Eq, Ord, Read)

instance Show Index where
  show i = "<" ++ show i.column ++ ", " ++ show i.row ++ ">"

left :: Index -> Index
left i = i { column = i.column - 1 }

right :: Index -> Index
right i = i { column = i.column + 1 }

up :: Index -> Index
up i = i { row = i.row - 1 }

down :: Index -> Index
down i = i { row = i.row + 1 }

wraparound :: Int -> Index -> Index
wraparound s i = Index {column = i.column `mod` s, row = i.row `mod` s}

allIndices :: Size -> [Index]
allIndices (Size s) =
  [Index {column = c, row = r} | c <- [0 .. s - 1], r <- [0 .. s - 1]]

index :: forall a. Grid a -> Index -> Maybe a
index g i = (g !? i.row) >>= (!? i.column)

generate :: Size -> (Index -> a) -> Grid a
generate (Size s) f =
  Vector.generate s $ \r -> Vector.generate s $ \c -> f (Index c r)

indexed :: Grid a -> Grid (Index, a)
indexed g =
  Vector.map
    (\(r, xs) -> Vector.map (\(c, x) -> (Index c r, x)) (Vector.indexed xs))
    (Vector.indexed g)

shape :: Grid a -> Grid Index
shape = Grid.map fst . indexed

mapWithIndex :: forall a b. (Index -> a -> b) -> Grid a -> Grid b
mapWithIndex f = Grid.map (uncurry f) . indexed

replace :: forall a. Index -> a -> Grid a -> Grid a
replace i x = mapWithIndex (\j y -> if i == j then x else y)

winningIndices :: Int -> [[Index]]
winningIndices size = rows ++ columns ++ diagonals
  where
    rows =
      [ [Index {column = c, row = r} | c <- [0 .. size - 1]]
      | r <- [0 .. size - 1]
      ]
    columns =
      [ [Index {column = c, row = r} | r <- [0 .. size - 1]]
      | c <- [0 .. size - 1]
      ]
    diagonals =
      [ [Index {column = i, row = i} | i <- [0 .. size - 1]]
      , [Index {column = i, row = size - i - 1} | i <- [0 .. size - 1]]
      ]

gridSize :: Size -> Int
gridSize (Size n) = n
