module Day10 (answers) where

import Parser
import Data.List

-- | The test input file
testInputFile :: String
testInputFile = "res/Day10_test_input.txt"

-- | The input file
inputFile :: String
inputFile = "res/Day10_input.txt"

data Machine = Machine {
    lightDiagram        :: ![Char],
    wiringSchematics    :: ![[Int]],
    joltageRequirements :: ![Int]
} deriving Show

-- | Parses a list of integers
parseIntList :: String -> Parser [Int]
parseIntList delim = do
    _  <- parseIsOneOf delim
    ns <- some $ parseMore <|> parseOne
    _  <- parseIsOneOf delim
    return ns
    where
        parseMore = do 
            n <- parseInt
            _ <- parseChar ','
            return n
        parseOne  = parseInt

-- | Parses a single machine
parseMachine :: Parser Machine
parseMachine = do
    ld <- parseLightDiagram
    _  <- parseSpaces
    ws <- parseWiringSchematics
    _  <- parseSpaces
    jr <- parseJoltageRequirement
    _  <- parseSpaces
    return $ Machine { 
        lightDiagram = ld,
        wiringSchematics = ws,
        joltageRequirements = jr
        }
    where
        parseLightDiagram = do
            _  <- parseChar '['
            ld <- some $ parseIsOneOf ".#"
            _  <- parseChar ']'
            return ld
        parseWiringSchematics   = some $ parseSpaces >> parseIntList "()"
        parseJoltageRequirement = parseIntList "{}"

-- | Reads the input from the specified file
readInput :: String -> IO [Machine]
readInput file = do
    contents <- readFile file
    return $ runParser (some parseMachine) contents

-- | Turns the lights on the machine in the least number of moves
turnLightsOn1 :: Machine -> (Int, [[Char]])
turnLightsOn1 machine = turnLightsOnBFS1 moves end (0, [start]) where
    start = replicate (length (lightDiagram machine)) '.'
    end   = lightDiagram machine
    moves = wiringSchematics machine

-- | Toggles a light
toggleLight :: Char -> Char
toggleLight '.' = '#'
toggleLight  _  = '.'

-- | Applies the wiring to the specified light circuit
applyWiring :: [Char] -> [Int] -> [Char]
applyWiring ls ws = [if i `elem` ws then toggleLight l else l | i <- [0..length ls -1], let l = ls !! i]

-- | Applies the joltage 
--applyJoltage :: [Int] -> [Int] -> [Int]
--applyJoltage ls ws = [if i `elem` ws then l + 1 else l | i <- [0..length ls -1], let l = ls !! i]
applyJoltage :: [Int] -> [Int] -> [Int]
applyJoltage = zipWith (+)

-- | Returns if a circuit is under jolted
underJolted :: [Int] -> [Int] -> Bool
underJolted xs ys = and (zipWith (<=) ys xs)

-- | Makes a jolt map for the given wire schematic
mkJolts :: Int -> [Int] -> [Int]
mkJolts n ws = [if i `elem` ws then 1 else 0 | i <- [0..n - 1]]

-- | Turns lights on using breadth-first-search
turnLightsOnBFS1 :: [[Int]] -> [Char] -> (Int, [[Char]]) -> (Int, [[Char]])
turnLightsOnBFS1 moves end (k, circuits) = case find (==end) circuits of
    Just cs -> (k, [cs])
    Nothing -> turnLightsOnBFS1 moves end (k+1, circuits')
    where
        circuits' :: [[Char]]
        circuits' = nub [applyWiring c m | c <- circuits, m <- moves]

-- | Turns the lights on the machine in the least number of moves
turnLightsOn2 :: Machine -> (Int, [[Int]])
turnLightsOn2 machine = turnLightsOnBFS2 jolts end (0, [start]) where
    start = replicate (length (lightDiagram machine)) 0
    end   = joltageRequirements machine
    --jolts = (wiringSchematics machine)
    jolts = map (mkJolts (length start)) (wiringSchematics machine)

-- | Turns lights on using breadth-first-search (IF you have a century to wait)
turnLightsOnBFS2 :: [[Int]] -> [Int] -> (Int, [[Int]]) -> (Int, [[Int]])
turnLightsOnBFS2 jolts end (k, circuits) | k == 20   = error $ "Target : " ++ show end ++ "\nNew circuits : " ++ show circuits'
                                         | otherwise = case find (==end) circuits of
    Just cs -> (k, [cs])
    Nothing -> turnLightsOnBFS2 jolts end (k+1, circuits'')
    where
        circuits'' :: [[Int]]
        circuits'' = filter (underJolted end) circuits' 
        circuits' :: [[Int]]
        circuits'  = nub [applyJoltage c j | c <- circuits, j <- jolts]
 

-- | The answer to part1
answer1 :: [Machine] -> Int
answer1 input = sum minMoves where
    minMoves = map (fst . turnLightsOn1) input

-- | The answer to part2
answer2 :: [Machine] -> Int
answer2 input = 0-- sum minMoves where
    --minMoves = map (fst . turnLightsOn2) input

-- | The answers
answers :: IO [Int]
answers = do
    input <- readInput inputFile
    print $ underJolted [0,1,2,3] [0,1,2,3]
    print $ turnLightsOn2 (input !! 0)
    return [answer1 input, answer2 input]
