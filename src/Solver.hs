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
import Data.Ord
import Data.List (nubBy, intercalate, (\\), sortBy)
import Data.Maybe (catMaybes, listToMaybe)
import Control.Monad (foldM)
import Text.Printf

import Data.Matrix (fromLists)

data Solution = S [PieceSolution]

instance Show Solution where
    show (S tps) = "Solution:\n" ++ show tps

data PieceSolution = PS Piece Transform

instance Show PieceSolution where
    show (PS (P id bs) t) = "\nPiece " ++ show id ++ " with positions: ["
                    ++ (intercalate ", " $ map (\(B color pos) -> show (B color pos) ++ "->" ++ show (transform pos t)) bs)
                    ++ "]\n" ++ show t

type CCuboid = Cuboid Color

type PieceTransPos = (Piece, [TransPos])

type TransPos = (Transform, [(Color, Pos)])

analyzePuzzle :: Puzzle -> IO ()
analyzePuzzle p = do
        putStrLn $ "Is valid: " ++ show (verifyPuzzle p)
        putStrLn $ "Unique transforms:" ++ concatMap (\((P id _),uts) -> "\nPiece " ++ show id ++ " with " ++ show (length uts) ++ " transformations") uniqueTrans
    where (Puzzle ps size) = p
          uniqueTrans = uniqueTransforms size (emptyCube size) ps

verifyPuzzle :: Puzzle -> Bool
verifyPuzzle (Puzzle ps (x,y,z)) = length allBlocks == numBlocksToFill &&
        ((even numBlocksToFill && length whiteBlocks == length blackBlocks) ||
         (odd numBlocksToFill && 1 == abs (length whiteBlocks - length blackBlocks)))
    where numBlocksToFill = x*y*z
          allBlocks = concatMap (\(P _ bs) -> bs) ps
          whiteBlocks = filter (\(B color _) -> color == White) allBlocks
          blackBlocks = filter (\(B color _) -> color == Black) allBlocks

solve :: Puzzle -> IO (Maybe Solution)
solve p = fmap listToMaybe $ solveAll p

solveAll :: Puzzle -> IO [Solution]
solveAll (Puzzle ps size) = do
    ss <- solveAll' (length ts) p ts spsts c []
--    let ss = solveAll'' ((p,ts):spsts) c []
    putStr "\n"
    return ss
    where ((p,ts):spsts) = uniqueTransforms size c ps
          c = emptyCube size

solveAll' :: Int -> Piece -> [TransPos] -> [PieceTransPos] -> CCuboid -> [PieceSolution] -> IO [Solution]
solveAll' _ _ [] _ _ _ = return []
solveAll' n p ((t,bs):tbs) psts c pss = do
          printf "Computing... %.2f%%\n" percent
          let c' = insertBlocks bs c
          let ss = solveAll'' psts c' (PS p t:pss)
          case ss of
            [] -> remSols
            _  -> fmap (ss ++) remSols
    where percent = (1 - (fromIntegral $ length tbs) / (fromIntegral n)) * 100 :: Double
          remSols = solveAll' n p tbs psts c pss

solveAll'' :: [PieceTransPos] -> CCuboid -> [PieceSolution] -> [Solution]
solveAll'' [] _ pss = [S pss]
solveAll'' ((p, tbs):psts) c pss = concatMap (\(pp, c') -> solveAll'' psts c' (pp:pss)) $ places p tbs c

places :: Piece -> [TransPos] -> CCuboid -> [(PieceSolution, CCuboid)]
places p tbs c = [(PS p t, insertBlocks bs c) | (t, bs) <- ftbs]
    where testBlock = all (\(color,pos) -> (not $ has pos c) && verifyAdj pos color c)
          ftbs = filter (testBlock . snd) tbs

insertBlocks :: [(Color, Pos)] -> CCuboid -> CCuboid
insertBlocks [] c = c
insertBlocks ((color, pos):bs) c = insertBlocks bs $ set pos color c

verifyAdj :: Pos -> Color -> CCuboid -> Bool
verifyAdj pos color c = notElem color . catMaybes . map (\pos' -> get pos' c) $ adjPos pos

adjPos :: Pos -> [Pos]
adjPos (x,y,z) = [(x-1,y,z),(x+1,y,z),(x,y-1,z),(x,y+1,z),(x,y,z-1),(x,y,z+1)]

uniqueTransforms :: Size -> CCuboid -> [Piece] -> [PieceTransPos]
uniqueTransforms size c ps = sortBy (comparing $ length . snd) psts
    where psts = map (\p' -> (p', uniqueTransforms' size ts p')) ps
          ts = transforms $ getCandidates c

uniqueTransforms' :: Size -> [Transform] -> Piece -> [TransPos]
uniqueTransforms' size ts (P _ bs) = utbss
    where tbss = zip ts $ map (\t -> map (\(B color pos) -> (color, transform pos t)) bs) ts
          ftbss = filter ((all $ isValidPos size . snd) . snd) tbss
          utbss = nubBy (\(_,v1) (_,v2) -> (null $ v1 \\ v2) && (null $ v2 \\ v1)) $ ftbss

isValidPos :: Size -> Pos -> Bool
isValidPos (x,y,z) (px,py,pz) = px >= 0 && px < x && py >= 0 && py < y && pz >= 0 && pz < z
