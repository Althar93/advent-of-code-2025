module Day7 (answers) where

import Data.List

-- | The test input file
testInputFile :: String
testInputFile = "res/Day7_test_input.txt"

-- | The input file
inputFile :: String
inputFile = "res/Day7_input.txt"

-- | Reads the specified input file
readInput :: String -> IO [[Char]]
readInput file = do
    contents <- readFile file
    return $ lines contents

-- | Returns the start column
getStartColumn :: [Char] -> Int
getStartColumn xs = case elemIndex 'S' xs of
    Just i  -> i
    Nothing -> error "Could not find start"

-- | Returns all splitter columns
getSplitterColumns :: [Char] -> [Int]
getSplitterColumns xs = [i | i <- [0..length xs-1], xs !! i == '^']

-- | Trace lasers down one row
traceLasersRow :: (Int, [Int]) -> [Int] -> (Int, [Int])
traceLasersRow (_,lasers) splitters = (noofSplits, lasers') where
    noofSplits = length splits
    lasers'    = (newLasers `union` lasers) \\ splitters
    newLasers  = [i+j | i <- splits, j <- [-1,1]]
    splits     = [i | i <- lasers, i `elem` splitters]

-- | Traces the laser down
traceLasers :: [[Char]] -> [(Int, [Int])]
traceLasers xss = scanl traceLasersRow (0,[start]) splitters where
    start     = getStartColumn (head xss)
    splitters = map getSplitterColumns (tail xss)

-- | Traces the laser down for part 2
traceLasers2 :: [[Char]] -> [[(Int,Int)]]
traceLasers2 xss = scanl traceLasersRow2 [(start,1)] splitters where
    start = getStartColumn (head xss)
    splitters = map getSplitterColumns (tail xss)

-- | Traces the laser down one row for part 2
traceLasersRow2 :: [(Int,Int)] -> [Int] -> [(Int,Int)]
traceLasersRow2 lasers splitters = lasers' where
    lasers'   = combineLasers (newLasers ++ others)
    others    = [(l,n)   | (l,n) <- lasers, l `notElem` splitters]
    newLasers = [(l+k,n) | (l,n) <- splits, k <- [-1,1]] 
    splits    = [(l,n)   | (l,n) <- lasers, l `elem` splitters]

-- | Combines multiple laser entries togethre
combineLasers :: [(Int, Int)] -> [(Int, Int)]
combineLasers lasers = combineLasers' (sort lasers) where
    combineLasers' :: [(Int, Int)] -> [(Int, Int)]
    combineLasers' []  = []
    combineLasers' [l] = [l]
    combineLasers' ((l0,n0):(l1,n1):ls) | l0 == l1  = combineLasers' ((l0,n0 + n1):ls)
                                        | otherwise = (l0,n0) : combineLasers' ((l1,n1):ls)
    
-- | Draws the room
drawRoom :: [[Char]] -> IO ()
drawRoom = mapM_ putStrLn

-- | The answer to part 1
answer1 :: [[Char]] -> Int
answer1 input = sum $ map fst (traceLasers input)

-- | The answer to part 2
answer2 :: [[Char]] -> Int
answer2 input = sum $ map snd ((last . traceLasers2) input)

-- | Answers
answers :: IO [Int]
answers = do
    input <- readInput inputFile
    return [answer1 input, answer2 input]
