-- | Types and functions for shapes. The list of all tetris pieces.
module Shapes where

import Data.List (transpose)
import Data.Maybe (isNothing)
import Test.QuickCheck

-- * Shapes

type Square = Maybe Colour

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
  deriving (Eq, Bounded, Enum, Show)

-- | A geometric shape is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.
data Shape = S [Row]
  deriving (Eq)

type Row = [Square]

rows :: Shape -> [Row]
rows (S rs) = rs

-- * Showing shapes

showShape :: Shape -> String
showShape s = unlines [showRow r | r <- rows s]
  where
    showRow :: Row -> String
    showRow r = [showSquare s | s <- r]

    showSquare Nothing = '.'
    showSquare (Just Black) = '█' -- can change to '█' on linux/mac -- #
    showSquare (Just Grey) = '▓' -- can change to '▓' -- g
    showSquare (Just c) = head (show c)

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss) ++ r

-- * The shapes used in the Tetris game

-- | All 7 tetrominoes (all combinations of 4 connected blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [S (makeSquares s) | s <- shapes]
  where
    makeSquares = map (map colour)
    colour c =
      lookup
        c
        [ ('I', Red),
          ('J', Grey),
          ('T', Blue),
          ('O', Yellow),
          ('Z', Cyan),
          ('L', Green),
          ('S', Purple)
        ]
    shapes =
      [ [ "I",
          "I",
          "I",
          "I"
        ],
        [ " J",
          " J",
          "JJ"
        ],
        [ " T",
          "TT",
          " T"
        ],
        [ "OO",
          "OO"
        ],
        [ " Z",
          "ZZ",
          "Z "
        ],
        [ "LL",
          " L",
          " L"
        ],
        [ "S ",
          "SS",
          " S"
        ]
      ]

-- * Some simple functions

-- ** A1

emptyShape :: (Int, Int) -> Shape
emptyShape (w, h) = S (replicate h (replicate w emptyBlock))
  where
    emptyBlock :: Square
    emptyBlock = Nothing

-- ** A2

-- | The size (width and height) of a shape
shapeSize :: Shape -> (Int, Int)
shapeSize (S rs) = (length (head rs), length rs)

-- ** A3

-- | Count how many non-empty squares a shape contains
blockCount :: Shape -> Int
blockCount (S rows) = length (filter (/= Nothing) flat)
  where
    flat = concat rows

-- * The Shape invariant

-- ** A4

-- | Shape invariant (shapes have at least one row, at least one column,
-- and are rectangular)

-- Uses average rather than first to not crash due to out of bounds index.
--  This shouldn't happen anyway if we can trust lazy evaluation to not do
--  anything till it's needed but better safe than sorry. And this is thread
--  safe! (as if any threading engine would thread this)
prop_Shape :: Shape -> Bool
prop_Shape (S rows)
  | null rows = False
  | row_length_avg == 0 = False
  | otherwise = all (== row_length_avg) row_lengths
  where
    row_lengths = map length rows
    row_length_avg = div (sum row_lengths) (length rows)

-- * Test data generators

-- ** A5

-- | A random generator for colours
genColour :: Gen Colour
genColour = do
  Positive number <- arbitrary
  return (toEnum (mod number 8))

instance Arbitrary Colour where
  arbitrary = genColour

-- ** A6

-- | A random generator for shapes
genShape :: Gen Shape
genShape = do
  Positive number <- arbitrary
  return (allShapes !! mod number (length allShapes))

instance Arbitrary Shape where
  arbitrary = genShape

-- * Transforming shapes

-- ** A7

-- | Rotate a shape 90 degrees
rotateShape :: Shape -> Shape
rotateShape (S rows) = S (map reverse (transpose rows))

-- ** A8

-- | shiftShape adds empty squares above and to the left of the shape
shiftShape :: (Int, Int) -> Shape -> Shape
shiftShape (n, m) shape = shifth n (shiftv m shape)
  where
    shiftv :: Int -> Shape -> Shape
    shiftv n (S rows) = S ([replicate w Nothing | i <- [1 .. n]] ++ rows)
    shifth :: Int -> Shape -> Shape
    shifth n (S rows) = S [replicate n Nothing ++ row | row <- rows]
    (w, _) = shapeSize shape

-- ** A9

-- | padShape adds empty sqaure below and to the right of the shape
padShape :: (Int, Int) -> Shape -> Shape
padShape (w, h) s@(S rows) = S (map (++ padW) rows ++ emptyRows)
  where
    padW = replicate w Nothing
    (width, _) = shapeSize s
    (S emptyRows) = emptyShape (w + width, h)

-- ** A10

-- | pad a shape to a given size
padShapeTo :: (Int, Int) -> Shape -> Shape
padShapeTo (w, h) s = padShape (paddingHorizontal, paddingVertical) s
  where
    (width, height) = shapeSize s
    paddingHorizontal = max (w - width) 0
    paddingVertical = max (h - height) 0

-- * Comparing and combining shapes

-- ** B1

-- | Test if two shapes overlap
overlaps :: Shape -> Shape -> Bool
s1 `overlaps` s2 = error "A11 overlaps undefined"

-- ** B2

-- | zipShapeWith, like 'zipWith' for lists
zipShapeWith :: (Square -> Square -> Square) -> Shape -> Shape -> Shape
zipShapeWith = error "A12 zipShapeWith undefined"

-- ** B3

-- | Combine two shapes. The two shapes should not overlap.
-- The resulting shape will be big enough to fit both shapes.
combine :: Shape -> Shape -> Shape
s1 `combine` s2 = error "A13 zipShapeWith undefined"
