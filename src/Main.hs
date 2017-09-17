module Main where

import Puzzle
import Solver

main :: IO ()
main = do
        s <- solve mediumPuzzle;
        putStrLn $ show s;
--        ss <- solveAll mediumPuzzle;
--        putStrLn $ show $ length ss;
--        s <- solve oscarsPuzzle;
--        putStrLn $ show s;
--        ss <- solveAll oscarsPuzzle;
--        putStrLn $ show $ length ss;

