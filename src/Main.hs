module Main where

import Puzzle
import Solver
import System.TimeIt

main :: IO ()
main = analyzeAndSolve oscarsPuzzle
--main = analyzeAndSolve mediumPuzzle

analyzeAndSolve :: Puzzle -> IO ()
analyzeAndSolve p = do
    analyzePuzzle p
    timeIt $ putStrLn $ show $ solve p
    timeIt $ putStrLn $ show $ take 100 $ solveAll p
--    timeIt $ putStrLn $ show $ length $ solveAll p
