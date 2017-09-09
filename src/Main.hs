module Main where

import Data.Maybe (catMaybes)

data Solution = S [PPiece] | Unsolvable

data PPiece = PP Piece Pos Transform

type Transform = (Integer, Integer, Integer)

type Cube = [[CBlock]]

data CBlock = None | Color


data Puzzle = Puzzle [Piece] Size

data Piece = P String [Block]

data Block = B Color Pos

data Color = White | Black

type Size = (Integer, Integer, Integer)
type Pos = (Integer, Integer, Integer)

puzzle :: Puzzle
puzzle = Puzzle [P "1" [B White (0,0,0)], P "2" [B White (0,0,0)], P "3" [B White (0,0,0)], P "4" [B White (0,0,0)],
                 P "5" [B Black (0,0,0)], P "6" [B Black (0,0,0)], P "7" [B White (0,0,0)], P "8" [B White (0,0,0)]] (2,2)

transforms :: [Transform]
transforms = [(x,y,z) | x <- [0..3], y <- [0..3], z <- [0..3]]

emptyCube :: Size -> Cube
emptyCube (x,y,z) = [[[None | _ <- [0..(x-1)]] | _ <- [0..(y-1)]] | _ <- [0..(z-1)]]

solve :: Puzzle -> Solution
solve (Puzzle ps size) = solve' ps size (emptyCube size) []

solve' :: [Piece] -> Size -> Cube -> [PPiece] -> Solution
solve' [] _ _ pps           = S pps
solve' (p:ps) size c pps    = case solutions of
                                    [] -> Unsolvable
                                    _  -> head solutions
    where solutions = filter (isSolution) $ map (\(pp, c) -> solve' ps size c (pp:pps)) $ places p size c

isSolution :: Solution -> Bool
isSolution (S _) = True
isSolution _     = False

places :: Piece -> Size -> Cube -> [(PPiece, Cube)]
places p (x,y,z) c = catMaybes [place p (px,py,pz) t c | px <- [0..(x-1)], py <- [0..(y-1)], pz <- [0..(z-1)], t <- transforms]

place :: Piece -> Pos -> Transform -> Cube -> Maybe (PPiece, Cube)
place p pos t c = case mcube of
                        Nothing -> Nothing
                        Just c  -> Just (PP p pos t, c)
    where (P _ bs) = p
          mcube    = place' bs pos t c

place' :: [Block] -> Pos -> Transform -> Cube -> Maybe Cube
place' [] _ _ = Just
place' (b:bs) = place'' b

place'' :: Block -> Pos -> Transform -> Cube -> Maybe Cube
place'' p (x,y,z) t c = undefined
    where (P _ bs) = p

main :: IO ()
main = putStrLn "Hello cube solver!"

-- | For P 1 (p1 : ps):
-- | Build cube with p1 in all positions with all orientations.
-- | For each:
-- |    If cube valid   -> If ps == []  -> Return solved cube
-- |                       else         -> Continue to build with cube and ps
-- |    else            -> Try next pos / orientations

