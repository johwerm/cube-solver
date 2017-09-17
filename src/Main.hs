module Main where

import Puzzle
import Solver

main :: IO ()
main = do
        putStrLn $ show $ solve oscarsPuzzle;
--        putStrLn $ show $ length $ solve oscarsPuzzle;
--        putStrLn $ show $ solve mediumPuzzle;
--        putStrLn $ show $ length $ solve mediumPuzzle;

