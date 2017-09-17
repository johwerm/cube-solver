module Main where

import Puzzle
import Solver

main :: IO ()
main = analyzeAndSolve oscarsPuzzle

analyzeAndSolve :: Puzzle -> IO ()
analyzeAndSolve p = do
    analyzePuzzle p
    putStrLn $ show $ solve p
--    putStrLn $ show $ length $ solveAll p
