module Main where

import Puzzle (simplePuzzle, oscarsPuzzle)
import Solver

main :: IO ()
main = putStrLn $ show $ solve oscarsPuzzle
