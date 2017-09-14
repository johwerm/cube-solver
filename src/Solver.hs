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
import Data.List (nubBy)
import Data.Maybe (catMaybes, listToMaybe)
import Control.Monad (foldM)

data Solution = S [PieceSolution]

instance Show Solution where
    show (S tps) = "Solution:\n" ++ show tps

data PieceSolution = PS Piece Transform

instance Show PieceSolution where
    show (PS ps t) = show ps ++ "\n" ++ show t

type CCuboid = Cuboid Color

solve :: Puzzle -> IO (Maybe Solution)
solve p = fmap listToMaybe $ solveAll p

solveAll :: Puzzle -> IO [Solution]
solveAll (Puzzle ps size) = solveAll' (length ts) p ts psts size c []
    where c = emptyCube size
          ((p,ts):psts) = map (\p' -> (p', uniqueTransforms size p' $ transforms $ getCandidates c)) ps

solveAll' :: Int -> Piece -> [Transform] -> [(Piece, [Transform])] -> Size -> CCuboid -> [PieceSolution] -> IO [Solution]
solveAll' _ _ [] _ _ _ _ = return []
solveAll' n p (t:ts) psts size c pss = do
          let (Just (ps,c')) = place p t c
          putStrLn $ "Computing... " ++ show percent ++ "%";
          let ss = solveAll'' psts size c' (ps:pss)
          case ss of
            [] -> solveAll' n p ts psts size c pss
            _  -> return ss
    where percent = (1 - (fromIntegral $ length ts) / (fromIntegral n)) * 100

solveAll'' :: [(Piece, [Transform])] -> Size -> CCuboid -> [PieceSolution] -> [Solution]
solveAll'' [] _ _ pss = [S pss]
solveAll'' ((p, ts):psts) size c pss = concatMap (\(pp, c) -> solveAll'' psts size c (pp:pss)) $ places p ts size c

places :: Piece -> [Transform] -> Size -> CCuboid -> [(PieceSolution, CCuboid)]
places p ts size c = catMaybes [place p t c | t <- ts]

uniqueTransforms :: Size -> Piece -> [Transform] -> [Transform]
uniqueTransforms size (P _ bs) ts = fst $ unzip utbss
    where bss = map (\t -> map (\(B color pos) -> (color, transform pos t)) bs) ts
          fbss = filter (all $ isValidPos size . snd) bss
          utbss = nubBy (\(_,v1) (_,v2) -> v1 == v2) $ zip ts fbss

isValidPos :: Size -> Pos -> Bool
isValidPos (x,y,z) (px,py,pz) = x >= 0 && x < px && y >= 0 && y < py && z >= 0 && z < pz

place :: Piece -> Transform -> CCuboid -> Maybe (PieceSolution, CCuboid)
place p t c = fmap (\c -> (PS p t, c)) $ place' bs t c
    where (P _ bs) = p

place' :: [Block] -> Transform -> CCuboid -> Maybe CCuboid
place' bs t c = foldM (\c b -> place'' b t c) c bs

place'' :: Block -> Transform -> CCuboid -> Maybe CCuboid
place'' (B color bpos) t c = case (mcolor, isAdjOk) of
                                   (Nothing, True) -> set bpos' color $ deleteCandidate bpos' c
                                   _               -> Nothing
    where bpos' = transform bpos t
          mcolor = get bpos' c
          isAdjOk = verifyAdj bpos' color c

verifyAdj :: Pos -> Color -> CCuboid -> Bool
verifyAdj pos color c = notElem color . catMaybes . map (\pos' -> get pos' c) $ adjPos pos

adjPos :: Pos -> [Pos]
adjPos (x,y,z) = [(x-1,y,z),(x+1,y,z),(x,y-1,z),(x,y+1,z),(x,y,z-1),(x,y,z+1)]
