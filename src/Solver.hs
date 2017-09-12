module Solver (
-- * Functions
    solve,
    solveAll,

-- * Types
    Solution,
) where

import Puzzle
import Transform
import Cuboid
import Data.Maybe (catMaybes, listToMaybe)
import Control.Monad (foldM)

data Solution = S [PieceSolution]

instance Show Solution where
    show (S tps) = "Solution:\n" ++ show tps

data PieceSolution = PS Piece Transform

instance Show PieceSolution where
    show (PS ps t) = show ps ++ "\n" ++ show t

type CCuboid = Cuboid Color

solve :: Puzzle -> Maybe Solution
solve = listToMaybe . solveAll

solveAll :: Puzzle -> [Solution]
solveAll (Puzzle ps size) = solveAll' ps size (emptyCube size) []

solveAll' :: [Piece] -> Size -> CCuboid -> [PieceSolution] -> [Solution]
solveAll' [] _ _ pss = [S pss]
solveAll' (p:ps) size c pss = concatMap (\(pp, c) -> solveAll' ps size c (pp:pss)) $ places p size c

places :: Piece -> Size -> CCuboid -> [(PieceSolution, CCuboid)]
places p size c = catMaybes [place p t c | t <- transforms $ getCandidates c]

place :: Piece -> Transform -> CCuboid -> Maybe (PieceSolution, CCuboid)
place p t c = fmap (\c -> (PS p t, c)) $ place' bs t c
    where (P _ bs) = p

place' :: [Block] -> Transform -> CCuboid -> Maybe CCuboid
place' bs t c = foldM (\c b -> place'' b t c) c bs

place'' :: Block -> Transform -> CCuboid -> Maybe CCuboid
place'' (B color bpos) t c = case (mcolor, isAdjOk) of
                                   (Nothing, True) -> Just $ set bpos' color $ deleteCandidate bpos' c
                                   _               -> Nothing
    where bpos' = transform bpos t
          mcolor = get bpos' c
          isAdjOk = verifyAdj bpos' color c

verifyAdj :: Pos -> Color -> CCuboid -> Bool
verifyAdj pos color c = notElem color . catMaybes . map (\pos' -> get pos' c) $ adjPos pos

adjPos :: Pos -> [Pos]
adjPos (x,y,z) = [(x-1,y,z),(x+1,y,z),(x,y-1,z),(x,y+1,z),(x,y,z-1),(x,y,z+1)]

