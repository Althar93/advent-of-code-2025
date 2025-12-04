module Day4 (answers) where

-- | The test input file
testInputFile :: String
testInputFile = "res/Day4_test_input.txt"

-- | The actual input
inputFile :: String
inputFile = "res/Day4_input.txt"

-- Abstract grid
type Grid = [[Char]]

-- | Reads the input
readInput :: String -> IO Grid
readInput file = do
    contents <- readFile file
    return $ lines contents

-- | Returns the width of the grid
width :: [[a]] -> Int
width = length . head

-- | Returns the height of the grid
height :: [[a]] -> Int
height = length

-- | Returns the neighbours of a cell
neighbourRolls :: Int -> Int -> [[Char]] -> [Char]
neighbourRolls _  _ []  = error "Empty list"
neighbourRolls x0 y0 gs = [n | x <- [xm..xM], y <- [ym..yM], not (x==x0 && y==y0), let n = (gs !! y) !! x, n == '@'] where
    (xm, xM) = (max 0 (x0 - 1), min (w - 1) (x0 + 1))
    (ym, yM) = (max 0 (y0 - 1), min (h - 1) (y0 + 1))
    (w, h)   = (width gs, height gs)

-- | Maps a function to a 2D grid
mapGrid :: (Int -> Int -> a -> b) -> [[a]] -> [[b]]
mapGrid f xss = [[f x y ((xss !! y) !! x) | x <- [0..w-1]] | y <- [0..h-1]] where
    w = width xss
    h = height xss

-- | List all removal locations
mapRemovables :: [[Char]] -> [[Bool]]
mapRemovables xss = mapGrid (\x y g -> g == '@' && length (neighbourRolls x y xss) < 4) xss

-- | Remove rolls
mapRemoveRolls :: [[Bool]] -> [[Char]] -> [[Char]]
mapRemoveRolls yss xss = mapGrid (\x y g -> if (yss !! y) !! x then 'x' else g) xss 

-- | Keep removing rolls until no longer possible
removeRolls :: [[Char]] -> Int
removeRolls xss = if noof > 0 then noof + removeRolls xss' else 0 where
    noof      = length $ concatMap (filter id) yss
    xss'      = mapRemoveRolls yss xss
    yss       = mapRemovables xss

-- | The answer for part 1
answer1 :: Grid -> Int
answer1 xss = length $ concatMap (filter id) (mapRemovables xss)

-- | The answer for part 2
answer2 :: Grid -> Int
answer2 xss = removeRolls xss 

-- | The answers
answers :: IO [Int]
answers = do
    input <- readInput inputFile
    return [answer1 input, answer2 input]
