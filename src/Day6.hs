module Day6 (answers) where

import Parser

import Data.Either
import Data.List

-- | The test input file
testInputFile :: String 
testInputFile = "res/Day6_test_input.txt"

-- | The input file
inputFile :: String
inputFile = "res/Day6_input.txt"

-- A single math problem
type Problem = (Char, [Int])

-- | My implementation of 'chunksOf' as found in Data.List.Split
chunksOf :: Int -> [a] -> [[a]]
chunksOf n = chunksOf' 0 [] where
    chunksOf' :: Int -> [a] -> [a] -> [[a]]
    chunksOf' _ ls [] = [ls]
    chunksOf' k ls (r:rs) | k == n    = ls : chunksOf' 0 [] (r:rs)
                          | otherwise = chunksOf' (k+1) (ls++[r]) rs
                                 
-- | Makes a list of problems given the sequence of operators and numbers
mkProblems1 :: [Char] -> [Int] -> [Problem]
mkProblems1 os ns = zip os (transpose (chunksOf (length os) ns))

-- | Makes a list of problems for part 2
mkProblems2 :: [String] -> [Problem]
mkProblems2 xss = map (\(c, is) -> (c, mkNumberLinesColumnRange is ns)) os where
     ns  = init xss
     os  = buildRange (length ls) ls'       :: [(Char, [Int])]
     ls' = filter (\(c, _) -> c /= ' ') ls  :: [(Char, Int)]
     ls  = zip (last xss) [0..]             :: [(Char, Int)]            

-- | Builds the range from one operator to the other
buildRange :: Int -> [(Char, Int)] -> [(Char, [Int])]
buildRange _ [] = []
buildRange k ((c,i):(d,j):rs) = (c,[i..j-2]) : buildRange k ((d,j):rs)
buildRange k [(c,i)]          = [(c,[i..k-1])]

-- | Makes a number from the given column
mkNumberLinesColumn :: Int -> [String] -> Int
mkNumberLinesColumn k xss = read [(xss !! y) !! k | y <- [0..length xss - 1]] :: Int

-- | Makes numbers from the given colum1
mkNumberLinesColumnRange :: [Int] -> [String] -> [Int]
mkNumberLinesColumnRange ks xss = map (`mkNumberLinesColumn` xss) ks

-- | Evaluates the problem
evaluateProblem :: Problem -> Int
evaluateProblem ('*', ns) = product ns
evaluateProblem ('+', ns) = sum ns
evaluateProblem p         = error $ "Not implemented for : " ++ show p

-- | Parses multiple problems
parseProblems :: Parser [Problem]
parseProblems = do
    ns <- parseNumbers
    os <- parseOperators
    return $ mkProblems1 os ns
    where
        parseNumbers   = some $ parseSpaces >> parseInt
        parseOperators = some $ parseSpaces >> parseIsOneOf "*+"

-- | Reads the input from the specified file
readInput1 :: String -> IO [Problem]
readInput1 file = do
    contents <- readFile file
    return $ runParser parseProblems contents

-- | Reads the input from the specified file
readInput2 :: String -> IO [Problem]
readInput2 file = do
    contents <- readFile file
    return $ mkProblems2 (lines contents)

-- | Answer to part 1
answer1 :: [Problem] -> Int
answer1 ps = sum $ map evaluateProblem ps

-- | Answer to part 2
answer2 :: [Problem] -> Int
answer2 ps = sum $ map evaluateProblem ps

-- | Answers
answers :: IO [Int]
answers = do
    let file = inputFile
    input1 <- readInput1 file
    input2 <- readInput2 file
    return [answer1 input1, answer2 input2]
