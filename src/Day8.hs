module Day8 (answers) where

import Parser

-- | The test input file
testInputFile :: String
testInputFile = "res/Day8_test_input.txt"

-- | The input file
inputFile :: String
inputFile = "res/Day8_input.txt"

-- | Position of a single junction box
type Pos = (Int, Int, Int)

parsePos :: Parser Pos
parsePos = do
    x <- parseInt
    _ <- parseChar ','
    y <- parseInt
    _ <- parseChar ','
    z <- parseInt
    _ <- parseSpaces
    return (x,y,z)

readInput :: String -> IO [Pos]
readInput file = do
    contents <- readFile file
    return $ runParser (some parsePos) contents

-- | Answer to part 1
answer1 :: [Pos] -> Int
answer1 ps = 0

-- | Answer to part2
answer2 :: [Pos] -> Int
answer2 ps = 0

-- | Answers
answers :: IO [Int]
answers = do
    input <- readInput testInputFile
    print input
    return [answer1 input, answer2 input]
