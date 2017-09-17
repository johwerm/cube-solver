module Puzzle (
-- * Functions
    simplePuzzle,
    simplePuzzle2,
    mediumPuzzle,
    oscarsPuzzle,

-- * Types
    Puzzle (Puzzle),
    Piece (P),
    Block (B),
    Color (Black, White),
) where

import Common

data Puzzle = Puzzle [Piece] Size
    deriving Show

data Piece = P String [Block]

instance Show Piece where
    show (P id bs) = "\nPiece \"" ++ id ++ "\" with blocks: " ++ show bs

data Block = B Color Pos

instance Show Block where
    show (B color pos) = show color ++ ":" ++ show pos

data Color = White | Black
    deriving (Show, Eq)

simplePuzzle :: Puzzle
simplePuzzle = Puzzle [P "1" [B White (0,0,0)], P "2" [B White (0,0,0)], P "3" [B White (0,0,0)], P "4" [B White (0,0,0)],
                 P "5" [B Black (0,0,0)], P "6" [B Black (0,0,0)], P "7" [B Black (0,0,0)], P "8" [B Black (0,0,0)]] (2,2,2)

simplePuzzle2 :: Puzzle
simplePuzzle2 = Puzzle [
    P "1" [B White (0,0,0),B Black (1,0,0), B Black (0,1,0),B White (1,1,0)],
    P "2" [B White (0,0,0), B Black (1,0,0)],
    P "3" [B White (0,0,0)],
    P "4" [B Black (0,0,0)]] (2,2,2)

mediumPuzzle :: Puzzle
mediumPuzzle = Puzzle [
    P "1" [B White (0,0,0),B Black (1,0,0), B White (2,0,0),B Black (2,1,0)],
    P "2" [B White (0,0,0),B Black (1,0,0), B White (2,0,0),B Black (2,1,0)],
    P "3" [B White (0,0,0), B Black (1,0,0), B White (2,0,0)],
    P "4" [B White (0,0,0), B Black (0,1,0), B White (1,1,0), B Black (2,1,0), B White (2,0,0)],
    P "5" [B Black (0,0,0),B White (1,0,0), B Black (2,0,0),B White (2,1,0)],
    P "6" [B Black (0,0,0),B White (1,0,0), B Black (1,1,0)],
    P "7" [B White (0,0,0),B Black (1,0,0), B White (2,0,0),B Black (2,1,0)]] (3,3,3)

oscarsPuzzle :: Puzzle
oscarsPuzzle = Puzzle [
    P "1" [B Black (0,0,0), B White (0,1,0), B White (1,0,0), B Black (1,1,0), B Black (2,0,0)],
    P "2" [B White (0,0,0), B Black (1,0,0), B White (1,1,0), B Black (2,1,0), B White (1,-1,0)],
    P "3" [B White (0,0,0), B Black (1,0,0), B White (1,1,0), B Black (2,1,0), B White (3,1,0)],
    P "4" [B White (0,0,0), B Black (1,0,0), B White (2,0,0), B Black (0,1,0), B White (0,2,0)],
    P "5" [B Black (0,0,0), B White (1,0,0), B Black (1,-1,0), B White (2,-1,0), B Black (2,-2,0)],
    P "6" [B Black (0,0,0), B White (1,0,0), B White (0,1,0), B Black (1,1,0)],
    P "7" [B White (0,0,0), B Black (1,0,0), B White (1,1,0), B Black (1,2,0), B White (2,0,0)],
    P "8" [B White (0,0,0), B Black (0,1,0), B Black (1,0,0), B White (2,0,0), B Black (2,1,0)],
    P "9" [B Black (0,0,0), B White (1,0,0), B Black (2,0,0), B White (3,0,0), B Black (3,1,0)],
    P "10" [B White (0,0,0), B Black (1,0,0), B White (2,0,0), B Black (2,1,0), B Black (3,0,0)],
    P "11" [B Black (0,0,0), B White (1,0,0), B Black (2,0,0), B White (2,1,0), B Black (3,1,0)],
    P "12" [B White (0,0,0), B Black (1,0,0), B White (1,1,0), B White (1,-1,0), B White (2,0,0)],
    P "13" [B White (0,0,0), B Black (0,1,0), B Black (1,0,0), B White (2,0,0), B Black (2,-1,0)]]
    (4,4,4)

