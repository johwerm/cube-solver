module Main where

import Transform
import Cube
import Data.Vector
import Data.Maybe (catMaybes)
import Control.Monad (foldM)

data Solution = S [TPiece] | Unsolvable

data TPiece = TP Piece Transform

type CCuboid = Cube (Maybe Color)


data Puzzle = Puzzle [Piece] Size

data Piece = P String [Block]

data Block = B Color Pos

data Color = White | Black

puzzle :: Puzzle
puzzle = Puzzle [P "1" [B White (0,0,0)], P "2" [B White (0,0,0)], P "3" [B White (0,0,0)], P "4" [B White (0,0,0)],
                 P "5" [B Black (0,0,0)], P "6" [B Black (0,0,0)], P "7" [B White (0,0,0)], P "8" [B White (0,0,0)]] (2,2)

solve :: Puzzle -> Solution
solve (Puzzle ps size) = solve' ps size (emptyCube size) []

solve' :: [Piece] -> Size -> CCuboid -> [TPiece] -> Solution
solve' [] _ _ pps           = S pps
solve' (p:ps) size c pps    = case solutions of
                                    [] -> Unsolvable
                                    _  -> head solutions
    where solutions = filter (isSolution) $ map (\(pp, c) -> solve' ps size c (pp:pps)) $ places p size c

isSolution :: Solution -> Bool
isSolution (S _) = True
isSolution _     = False

places :: Piece -> Size -> CCuboid -> [(TPiece, CCuboid)]
places p size c = catMaybes [place p t c | t <- transforms size]

place :: Piece -> Transform -> CCuboid -> Maybe (TPiece, CCuboid)
place p t c = fmap (\c -> (PP p t, c)) $ place' bs t c
    where (P _ bs) = p

place' :: [Block] -> Transform -> CCuboid -> Maybe CCube
place' bs t c = foldM (\c b -> place'' b t c) c bs

place'' :: Block -> Transform -> CCuboid -> Maybe CCube
place'' (B color bpos) t c = undefined
    where bpos' = transform bpos t

main :: IO ()
main = putStrLn "Hello cube solver!"

-- | For P 1 (p1 : ps):
-- | Build cube with p1 in all positions with all orientations.
-- | For each:
-- |    If cube valid   -> If ps == []  -> Return solved cube
-- |                       else         -> Continue to build with cube and ps
-- |    else            -> Try next pos / orientations

