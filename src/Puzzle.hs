{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Puzzle (
-- * Functions
    verifyPuzzle,

-- * Constants
    trivialPuzzle,
    easyPuzzle,
    mediumPuzzle,
    oscarsPuzzle,

-- * Types
    Puzzle (Puzzle),
    Piece (P),
    Block (B),
    Color, white, black,
) where

-- | Imports

import Common

-- | Types

data Puzzle = Puzzle [Piece] Size
    deriving Show

data Piece = P String [Block]

instance Show Piece where
    show (P id bs) = "\nPiece \"" ++ id ++ "\" with blocks: " ++ show bs

data Block = B Color Pos

instance Show Block where
    show (B color pos) = show color ++ ":" ++ show pos

newtype Color = Color Int deriving (Eq,Ord,Enum)
(white:black:_) = [Color 1, Color 2]

instance Show Color where
    show (Color i) | i == 1 = "White"
                   | i == 2 = "Black"

-- | Puzzle verification functions

verifyPuzzle :: Puzzle -> Bool
verifyPuzzle (Puzzle ps (x,y,z)) = length allBlocks == numBlocksToFill &&
        ((even numBlocksToFill && length whiteBlocks == length blackBlocks) ||
         (odd numBlocksToFill && 1 == abs (length whiteBlocks - length blackBlocks)))
    where numBlocksToFill = x*y*z
          allBlocks = concatMap (\(P _ bs) -> bs) ps
          whiteBlocks = filter (\(B color _) -> color == white) allBlocks
          blackBlocks = filter (\(B color _) -> color == black) allBlocks

-- | Example puzzles

trivialPuzzle :: Puzzle
trivialPuzzle = Puzzle [P "1" [B white (0,0,0)], P "2" [B white (0,0,0)], P "3" [B white (0,0,0)], P "4" [B white (0,0,0)],
                 P "5" [B black (0,0,0)], P "6" [B black (0,0,0)], P "7" [B black (0,0,0)], P "8" [B black (0,0,0)]] (2,2,2)

easyPuzzle :: Puzzle
easyPuzzle = Puzzle [
    P "1" [B white (0,0,0),B black (1,0,0), B black (0,1,0),B white (1,1,0)],
    P "2" [B white (0,0,0), B black (1,0,0)],
    P "3" [B white (0,0,0)],
    P "4" [B black (0,0,0)]] (2,2,2)

mediumPuzzle :: Puzzle
mediumPuzzle = Puzzle [
    P "1" [B white (0,0,0),B black (1,0,0), B white (2,0,0),B black (2,1,0)],
    P "2" [B white (0,0,0),B black (1,0,0), B white (2,0,0),B black (2,1,0)],
    P "3" [B white (0,0,0), B black (1,0,0), B white (2,0,0)],
    P "4" [B white (0,0,0), B black (0,1,0), B white (1,1,0), B black (2,1,0), B white (2,0,0)],
    P "5" [B black (0,0,0),B white (1,0,0), B black (2,0,0),B white (2,1,0)],
    P "6" [B black (0,0,0),B white (1,0,0), B black (1,1,0)],
    P "7" [B white (0,0,0),B black (1,0,0), B white (2,0,0),B black (2,1,0)]] (3,3,3)

oscarsPuzzle :: Puzzle
oscarsPuzzle = Puzzle [
    P "1" [B black (0,0,0), B white (0,1,0), B white (1,0,0), B black (1,1,0), B black (2,0,0)],
    P "2" [B white (0,0,0), B black (1,0,0), B white (1,1,0), B black (2,1,0), B white (1,-1,0)],
    P "3" [B white (0,0,0), B black (1,0,0), B white (1,1,0), B black (2,1,0), B white (3,1,0)],
    P "4" [B white (0,0,0), B black (1,0,0), B white (2,0,0), B black (0,1,0), B white (0,2,0)],
    P "5" [B black (0,0,0), B white (1,0,0), B black (1,-1,0), B white (2,-1,0), B black (2,-2,0)],
    P "6" [B black (0,0,0), B white (1,0,0), B white (0,1,0), B black (1,1,0)],
    P "7" [B white (0,0,0), B black (1,0,0), B white (1,1,0), B black (1,2,0), B white (2,0,0)],
    P "8" [B white (0,0,0), B black (0,1,0), B black (1,0,0), B white (2,0,0), B black (2,1,0)],
    P "9" [B black (0,0,0), B white (1,0,0), B black (2,0,0), B white (3,0,0), B black (3,1,0)],
    P "10" [B white (0,0,0), B black (1,0,0), B white (2,0,0), B black (2,1,0), B black (3,0,0)],
    P "11" [B black (0,0,0), B white (1,0,0), B black (2,0,0), B white (2,1,0), B black (3,1,0)],
    P "12" [B white (0,0,0), B black (1,0,0), B white (1,1,0), B white (1,-1,0), B white (2,0,0)],
    P "13" [B white (0,0,0), B black (0,1,0), B black (1,0,0), B white (2,0,0), B black (2,-1,0)]]
    (4,4,4)

