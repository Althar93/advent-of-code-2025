module Day11 (answers) where

import Parser
import Data.List
import Data.Bits
import Debug.Trace

-- | The test input file 
testInputFile :: String
testInputFile = "res/Day11_test_input.txt"

-- | The test input file for part 2
testInputFile2 :: String
testInputFile2 = "res/Day11_test_input_2.txt"

-- | The input file
inputFile :: String
inputFile = "res/Day11_input.txt"

-- | A basic device type
type Device  = (String, [String])

-- | Expanded device with pre-computed paths and hashed ids instead of strings
type Hash    = Int
type Device' = (Hash, [[Hash]])

-- | Parses an id
parseId :: Parser String
parseId = some $ parseIsOneOf "abcedfghijklmnopqrstuvwxyz"

-- | Parses a device
parseDevice :: Parser Device
parseDevice = do
    s  <- parseId
    _  <- parseString ": "
    ns <- some $ parseMore <|> parseId
    _  <- parseSpaces
    return (s, ns)
    where
        parseMore = do
            x <- parseId
            _ <- parseSpaces
            return x

-- | Reads the input from the specified file
readInput :: String -> IO [Device]
readInput file = do
    contents <- readFile file
    return $ map (runParser parseDevice) (lines contents)

-- | Finds the named device
findDevice :: (Eq a, Show a) => [(a,b)] -> a -> (a,b)
findDevice xs y = case find ((==y) . fst) xs of
    Nothing -> error $ "Could not find " ++ show y
    Just x  -> x

-- | Resolve a path to its end point (out or branch)
resolvePath :: [Device] -> [String] -> [String]
resolvePath devices path | head path == "out" = path -- Done
                         | otherwise = case snd $ findDevice devices (head path) of
                            -- Keep resolving path
                            [x] -> path--resolvePath devices (x:path)
                            -- Let's not resolve multi paths for now
                            xs  -> path

-- | Hash using the DJB2
hash :: String -> Int
hash = foldl' (\h c -> 33*h `xor` fromEnum c) 5381

-- | Makes an expanded device
mkDevice' :: [Device] -> [Device']
mkDevice' ds = map (\(x, ys) -> (hash x, map (\y -> map hash (resolvePath ds [y])) ys)) ds

-- | Finds all the paths from the start to end using the specified devices
findAllPaths :: [Device] -> String -> [[String]] -> [[String]]
findAllPaths _  _   []     = []
findAllPaths ds end paths  = endPaths ++ findAllPaths ds end newPaths where
    -- Work out end paths & others we need to keep tracing
    (endPaths, otherPaths) = partition ((==end) . head) paths
    -- Generate new paths
    newPaths               = nub $ [if null n then [] else n:p | p <- otherPaths, n <- validPaths p]
        where 
            -- | Returns all possible paths (which haven't been encountered before)
            validPaths :: [String] -> [String]
            validPaths (x:xs) | x == "out" = []
                              | otherwise  = filter (`notElem` xs) (snd (findDevice ds x))
            validPaths []     = error "Need at least one device"

-- | Finds all the paths from the start to end using the specified expanded devices
findAllPaths' :: [Device'] -> Hash -> [[Hash]] -> [[Hash]]
findAllPaths' _  _   []    = []
findAllPaths' ds end paths = endPaths ++ findAllPaths' ds end (map snd newPaths') where
    -- Work out end paths & others we need to keep tracing
    (endPaths, otherPaths) = partition ((==end) . head) paths
    -- Can we cull paths which share the same destination? 
    newPaths' :: [(Int, [Hash])]
    newPaths'              = map (\xs -> (length xs, head xs)) $ groupBy (\a b -> head a == head b) (sort newPaths)
    -- Generate new paths
    newPaths               = nub $ [if null n then [] else n++p | p <- otherPaths, n <- validPaths' p]
        where 
            -- | Returns all possible paths (which haven't been encountered before)
            validPaths' :: [Hash] -> [[Hash]]
            validPaths' (x:xs) | x == hash "out" = []
                               | otherwise       = filter (disjointed xs) (snd (findDevice ds x))
            validPaths' [] = error "Need at least one device"

-- Returns whether the two lists are disjointed
disjointed :: (Eq a) => [a] -> [a] -> Bool
disjointed xs = null . intersect xs

-- | The answer to part 1
answer1 :: [Device] -> Int
answer1 input = 0 --length $ findAllPaths input "out" [] [["you"]]

-- | The answer to part 2
answer2 :: [Device] -> Int
answer2 input = length $ filter (\ps -> all (`elem` ps) toVisit) allPaths where
    allPaths = findAllPaths' input' (hash "out") [[hash "svr"]]
    toVisit  = map hash ["dac", "fft"]
    input'   = mkDevice' input

-- | The answers
answers :: IO [Int]
answers = do
    input <- readInput inputFile
    let input' = mkDevice' input
    --mapM_ print input'
    --mapM_ print input
    --print $ findAllPaths  input  "out"        [["you"]]
    --print $ findAllPaths' input' (hash "out") [[hash "you"]]
    --print $ findAllPaths input ("dac") [["fft"]]
   
    
    print $ findAllPaths' input' (hash "dac") [[hash "fft"]]
    
    --print $ show $ map head $ groupBy (\a b -> head a == head b) [[0,1,2],[0,4,5,6,7,2],[3,1,2]]
    --print $ findAllPaths input ("out") [["dac"]]
    --print input
    --print $ resolvePath input ["eee"]
    --mapM_ print $ mkDevice' input
    return []--[answer1 input, answer2 input]
