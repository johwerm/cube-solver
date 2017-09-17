module Solver (
-- * Functions
    solve,
    solveAll,
    analyzePuzzle,

-- * Types
    Solution,
) where

-- | Imports

import Puzzle
import Transform
import Cuboid
import Data.Ord
import Data.List (nubBy, intercalate, (\\), sortBy)
import Data.Maybe (catMaybes, listToMaybe)
import Text.Printf

import System.TimeIt
-- | Types

data Solution = S [PieceSolution]

instance Show Solution where
    show (S tps) = "Solution:\n" ++ show tps

data PieceSolution = PS Piece Transform

instance Show PieceSolution where
    show (PS (P id bs) t) = "\nPiece " ++ show id ++ " with positions: ["
                    ++ (intercalate ", " $ map (\(B color pos) -> show (B color pos) ++ "->" ++ show (transform pos t)) bs)
                    ++ "]\n"

type CCuboid = Cuboid Color

type PieceTransPos = (Piece, [TransPos])

type TransPos = (Transform, [(Color, Pos)])

-- | Puzzle solver

solve :: Puzzle -> Maybe Solution
solve = listToMaybe . solveAll

solveAll :: Puzzle -> [Solution]
solveAll (Puzzle ps size) = solveAll' ws c [] ++ solveAll' bs c []
    where (ws,bs) = colorUniqueTransforms size ps
          c = empty size

solveAll' :: [PieceTransPos] -> CCuboid -> [PieceSolution] -> [Solution]
solveAll' [] _ pss = [S pss]
solveAll' ((p, tbs):psts) c pss = concatMap (\(pp, c') -> solveAll' psts c' (pp:pss)) $ places p tbs c

places :: Piece -> [TransPos] -> CCuboid -> [(PieceSolution, CCuboid)]
places p tbs c = [(PS p t, insertBlocks bs c) | (t, bs) <- filter (testBlock . snd) tbs]
--    where testBlock = all (\(color,pos) -> (not $ has pos c) && verifyAdj pos color c)
    where testBlock = all (\(color,pos) -> not $ has pos c)

insertBlocks :: [(Color, Pos)] -> CCuboid -> CCuboid
insertBlocks [] = id
insertBlocks ((color, pos):bs) = insertBlocks bs . set pos color

verifyAdj :: Pos -> Color -> CCuboid -> Bool
verifyAdj pos color c = notElem color . catMaybes . map (\pos' -> get pos' c) $ adjPos pos
    where adjPos (x,y,z) = [(x-1,y,z),(x+1,y,z),(x,y-1,z),(x,y+1,z),(x,y,z-1),(x,y,z+1)]

-- | Transformation helper functions

colorUniqueTransforms :: Size -> [Piece] -> ([PieceTransPos],[PieceTransPos])
colorUniqueTransforms size ps = ((map (\(p, ts) -> (p, filterByZColor White ts)) utbss) , (map (\(p, ts) -> (p, filterByZColor Black ts)) utbss))
    where utbss = uniqueTransforms size ps
          filterByZColor zcolor ts' = filter (\(_, bs) -> all (\(color, (x,y,z)) -> not $ (zcolor == color) /= (even (x+y+z))) bs) ts'

uniqueTransforms :: Size -> [Piece] -> [PieceTransPos]
uniqueTransforms size ps = sortBy (comparing $ length . snd) psts
    where psts = map (\p' -> (p', uniqueTransforms' size ts p')) ps
          ts = transforms $ getAllPositions size

uniqueTransforms' :: Size -> [Transform] -> Piece -> [TransPos]
uniqueTransforms' size ts (P _ bs) = utbss
    where tbss = zip ts $ map (\t -> map (\(B color pos) -> (color, transform pos t)) bs) ts
          ftbss = filter ((all $ isValidPos size . snd) . snd) tbss
          utbss = nubBy (\(_,v1) (_,v2) -> (null $ v1 \\ v2) && (null $ v2 \\ v1)) $ ftbss

isValidPos :: Size -> Pos -> Bool
isValidPos (x,y,z) (px,py,pz) =
    px >= 0 && px < x &&
    py >= 0 && py < y &&
    pz >= 0 && pz < z

-- | Puzzle analyzer functions.

analyzePuzzle :: Puzzle -> IO ()
analyzePuzzle p = do
        putStrLn $ "Is valid: " ++ show (verifyPuzzle p)
        putStrLn $ "Unique transforms:"
            ++ concatMap (\(((P id _), ws), (_,bs)) ->
                "\nPiece " ++ show id ++ " with (" ++
                show (length ws) ++ "+" ++
                show (length bs) ++") transformations") zutbss
            ++ "\nCombinations: " ++ printf "%.2E" (fromInteger $ wp + bp :: Float)
    where (Puzzle ps size) = p
          utbss = colorUniqueTransforms size ps
          zutbss = zip (fst utbss) (snd utbss)
          wp = product $ map (toInteger . length . snd) $ fst utbss
          bp = product $ map (toInteger . length . snd) $ snd utbss
