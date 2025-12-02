module Main (main) where

import Common
import Day1
import Day2

main :: IO ()
main = do
    evaluateAndPrint "Day 1" Day1.answers
    evaluateAndPrint "Day 2" Day2.answers
