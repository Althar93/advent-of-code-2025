module Day1 (answers) where

import Parser

testInputFile :: String
testInputFile = "res/Day1_test_input.txt"

inputFile :: String
inputFile = "res/Day1_input.txt"

parseRotations :: Parser [Int]
parseRotations = many parseRotation where
    parseRotation = do
       d <- parseItem
       n <- parseInt
       _ <- parseSpaces
       return $ if d == 'L' then -n else n 

readInput :: String -> IO [Int]
readInput file = do
    content <- readFile file
    return $ runParser parseRotations content

-- | Turns the dial by a fixed number of clicks
turn :: Int -> Int -> Int -> Int
turn n x0 r = (x0 + r) `mod` n

-- | Counts the number of rotations
rotations :: Int -> Int -> Int
rotations x r = f + p where
    f  = abs r `div` 100
    p  | x  == 0         = 0
       | x' == 0         = 1 
       | r > 0 && x' < x = 1
       | r < 0 && x' > x = 1
       | otherwise       = 0
    x' = turn 100 x r

-- | Answer for part 1
part1 :: [Int] -> Int
part1 yss = part1' 50 0 yss where
    part1' _ n []     = n
    part1' x n (y:ys) = part1' x' n' ys where
        x' = turn 100 x y
        n' = if x' == 0 then n + 1 else n

-- | Answer for part 2
part2 :: [Int] -> Int
part2 yss = part2' 50 0 yss where
    part2' _ n []     = n
    part2' x n (y:ys) = part2' x' n' ys where
        x' = turn 100 x y
        n' = n + rotations x y 

-- | Part 1 more compact & using 'scanl'
part1' :: [Int] -> Int
part1' yss = length $ filter (==0) xss where
    xss = scanl (\x y -> (x + y) `mod` 100) 50 yss
   
-- | Part 2 more compact & using 'scanl'
part2' :: [Int] -> Int
part2' yss = sum $ zipWith delta xss yss where
    xss = scanl (\x y -> (x + y) `mod` 100) 50 yss
    delta :: Int -> Int -> Int
    delta x y | y < 0     = ((100 - x) `mod` 100 - y) `div` 100
              | otherwise = (x + y) `div` 100

-- | Answers
answers :: IO [Int]
answers = do
    input <- readInput inputFile
    return [part1' input, part2' input]
