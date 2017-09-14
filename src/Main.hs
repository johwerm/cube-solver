module Main where

import Puzzle (simplePuzzle, oscarsPuzzle)
import Solver

main :: IO ()
main = do
        s <- solve oscarsPuzzle;
        putStrLn $ show s;
        ss2 <- solve simplePuzzle;
        putStrLn $ show $ length ss2;


