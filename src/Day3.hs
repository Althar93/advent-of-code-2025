module Day3 (answers) where

import Parser

import Data.Char

-- | The test input
testInputFile :: String
testInputFile = "res/Day3_test_input.txt"

-- | The actual input
inputFile::String
inputFile = "res/Day3_input.txt"

-- | Parse multiple banks
parseBanks :: Parser [[Int]]
parseBanks = some parseBank where
    parseBank = do
        _ <- parseSpaces
        ns <- some parseDigit
        return $ map digitToInt ns

-- | Reads the input
readInput :: String -> IO [[Int]]
readInput file = do
    contents <- readFile file
    return $ runParser parseBanks contents

-- | Drops the tail elements
dropTail :: Int -> [a] -> [a]
dropTail n xs = take (length xs - n) xs

-- | Generalised joltage calculation which takes the number of digits for the bank
maxJoltageR :: Int -> [Int] -> Int
maxJoltageR k b = sum $ zipWith (\n i -> n * 10^i) ls [k-1, k-2..0] where 
    ls = maxJoltageR' k 0
    maxJoltageR' 0  _ = []
    maxJoltageR' k' i | show rb ++ " : " ++ show (n, i')
                      | otherwise = n : maxJoltageR' (k' - 1) (i' + 1 + length lb) where
        (n, i')  = localMaximum (dropTail (k' - 1) rb)
        (lb, rb) = splitAt i b

-- | Returns the local maximum for a list of numbers
localMaximum :: [Int] -> (Int, Int)
localMaximum b = localMaximum' (0,0) 0 b where
    localMaximum' (m,k) _ []     = (m, k)
    localMaximum' (m,k) i (n:ns) | n > m     = localMaximum' (n, i) (i+1) ns
                                 | otherwise = localMaximum' (m, k) (i+1) ns

-- | Answer for part 1
answer1 :: [[Int]] -> Int
answer1 bs = sum $ map (maxJoltageR 2) bs

-- | Answer for part 2
answer2 :: [[Int]] -> Int
answer2 bs = sum $ map (maxJoltageR 12) bs

-- | Answers
answers :: IO [Int]
answers = do
    input <- readInput inputFile
    return [answer1 input, answer2 input]
