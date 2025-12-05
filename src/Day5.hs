module Day5 (answers) where

import Parser

import Data.Either
import Data.List

-- | The test input file
testInputFile :: String
testInputFile = "res/Day5_test_input.txt"

-- | The input file
inputFile :: String
inputFile = "res/Day5_input.txt"

parseIngredients :: Parser ([(Int, Int)], [Int])
parseIngredients = do
    es <- some $ parseEither parseRange parseIngredient
    return (lefts es, rights es)
    where
        parseRange = do
            x0 <- parseInt
            _  <- parseChar '-'
            xn <- parseInt
            _  <- parseSpaces
            return (x0,xn)
        parseIngredient = do
            x <- parseInt
            _ <- parseSpaces
            return x

-- | Reads the specified input
readInput :: String -> IO ([(Int, Int)], [Int])
readInput file = do
    contents <- readFile file
    return $ runParser parseIngredients contents

-- | Answer to part 1
answer1 :: ([(Int, Int)], [Int]) -> Int
answer1 (rs, as) = length $ filter (\a -> any id (map (\(xm, xM) -> a >= xm && a <= xM) rs)) as

-- | Merges ranges together
mergeRange :: [(Int,Int)] -> [(Int,Int)]
mergeRange []               = []
mergeRange ((xm,xM):(ym,yM):rs) | ym >= xm && yM <= xM            = mergeRange ((xm,xM):rs)         -- Fully contained
                                | ym >= xm && ym <= xM && yM > xM = mergeRange ((xm,yM):rs)         -- Extend range (y)
                                | xm >= ym && xm <= yM && xM > yM = mergeRange ((ym,xM):rs)         -- Extend range (x)
                                | otherwise                       = (xm,xM):mergeRange ((ym,yM):rs) -- Continue 
mergeRange x = x

-- | Merges range together recursively
mergeRangeR :: [(Int,Int)] -> [(Int,Int)]
mergeRangeR rs = if rs == rs' then rs else mergeRangeR rs'' where
    rs'' = mergeRange rs'
    rs'  = sortOn fst rs 

-- | Answer to part 2
answer2 :: ([(Int, Int)], [Int]) -> Int
answer2 (rs, _) = sum $ map (\(xm,xM) -> xM - xm + 1) (mergeRangeR rs)

--  Answers
answers :: IO [Int]
answers = do
    input <- readInput inputFile
    return [answer1 input, answer2 input]
