module Day12 (answers) where
import Parser
import Data.List

-- | The test input file
testInputFile :: String
testInputFile = "res/Day12_test_input.txt"

-- | The input file
inputFile :: String
inputFile = "res/Day12_input.txt"

-- | Convenience types
type Present = [[Char]]
type Region  = (Int, Int, [Int])
type Input   = ([Present], [Region])

-- | Parses a single present
parsePresent :: Parser Present
parsePresent = do
    _  <- parseInt
    _  <- parseChar ':'
    _  <- parseSpaces
    xs <- some parseRow
    _  <- parseSpaces
    return xs
    where
        parseRow = do
            s <- some $ parseIsOneOf "#."
            _ <- parseSpaces
            return s

-- | Parses a single region
parseRegion :: Parser Region
parseRegion = do
    x <- parseInt
    _ <- parseChar 'x'
    y <- parseInt
    _ <- parseString ": "
    xs <- some $ parseMoreInt <|> parseInt
    _ <- parseSpaces
    return (x, y, xs)
    where
        parseMoreInt = do
            n <- parseInt
            _ <- parseChar ' '
            return n

-- | Parses the input
parseInput :: Parser Input
parseInput = do
    ps <- some parsePresent
    _  <- parseSpaces
    rs <- some parseRegion
    return (ps, rs)

-- | Reads the input from the specified file
readInput :: String -> IO Input
readInput file = do
    contents <- readFile file
    return $ runParser parseInput contents

-- | Returns all permutations for a given shape
moves :: [[Char]] -> [[[Char]]]
moves xs = nub [xs, hFlip xs, vFlip xs, rot 1 xs, rot 2 xs, rot 3 xs] 
    where
        hFlip = map reverse
        vFlip = reverse
        rot :: Int -> [[Char]] -> [[Char]]
        rot 1 ys = (transpose . reverse) ys
        rot n ys = (transpose . reverse) (rot (n - 1) ys)

-- Returns the volume of a present
volumePresent :: Present -> Int
volumePresent xs = length $ concatMap (filter (=='#')) xs

-- | Returns the volume of a region
volumeRegion :: Region -> Int
volumeRegion (x,y,_) = x*y

-- | Returns the minimum volume required
minVolume :: [Present] -> Region -> Int
minVolume ps (_,_,xs) = sum [(xs !! i) * volumePresent (ps !! i) | i <- [0..length xs -1]]

-- | Returns the maximum volume required
maxVolume :: [Present] -> Region -> Int
maxVolume _ (_,_,xs) = sum xs * 9

-- | Sorts regions based on those which are easy fits : (fits, not sure)
fitFast :: [Present] -> [Region] -> ([Region], [Region])
fitFast ps rs = partition (\r -> volumeRegion r >= maxVolume ps r) rs

-- | Sorts regions based on those which cannot fits : (not sure, no fit)
noFitFast :: [Present] -> [Region] -> ([Region], [Region])
noFitFast ps rs = partition (\r -> volumeRegion r < minVolume ps r) rs

-- | Draws the present
drawPresent :: [[Char]] -> IO ()
drawPresent = mapM_ print

-- | The answer to part 1
answer1 :: Input -> Int
answer1 input = length fits where
    (fits, notsure) = fitFast presents regions
    presents = fst input
    regions  = snd input

-- | The answer to part 2
answer2 :: Input -> Int
answer2 input = 0

-- | The answer 
answers :: IO [Int]
answers = do
    input <- readInput inputFile
    return [answer1 input, answer2 input]
