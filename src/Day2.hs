module Day2 (answers) where

import Parser

-- | The test input
testInputFile :: String
testInputFile = "res/Day2_test_input.txt"

-- | The actual input
inputFile :: String
inputFile = "res/Day2_input.txt"

-- Parses many ranges
parseRanges :: Parser [(Int, Int)]
parseRanges = some $ parseMore <|> parseOne where
    parseMore = parseChar ',' >> parseOne
    parseOne  = do
        x <- parseInt
        _ <- parseChar '-'
        y <- parseInt
        return (x, y)

-- | Reads the input from a file
readInput :: String -> IO [(Int, Int)]
readInput file = do
    contents <- readFile file
    return $ runParser parseRanges contents

-- | Expand the id range
expandRanges :: (Int, Int) -> [Int]
expandRanges (x0,x1) = [x0..x1]

-- | Answer for part 1
answer1 :: [(Int, Int)] -> Int
answer1 xs = sum $ invalids where
    invalids  = filter isInvalid ranges 
    ranges    = concatMap expandRanges xs
    isInvalid x = (\(l,r) -> l == r) $ splitAt (length x' `div` 2) x' where
        x' = show x

-- | Answer for part 2
answer2 :: [(Int, Int)] -> Int
answer2 xs = sum $ invalids where
    invalids    = filter isInvalid ranges
    ranges      = concatMap expandRanges xs
    isInvalid x = (show x) `elem` [concat (replicate (length c `div` k) (take k c)) | let c = (show x), k <- [1..length c `div` 2]]

-- | Answers
answers :: IO [Int]
answers = do
    input <- readInput inputFile
    return [answer1 input, answer2 input]
