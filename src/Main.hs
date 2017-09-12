module Main where

import Puzzle (simplePuzzle, oscarsPuzzle)
import Solver

main :: IO ()
main = do
        putStrLn . show $ solve oscarsPuzzle;
        --putStrLn . show $ length $ solveAll oscarsPuzzle;


