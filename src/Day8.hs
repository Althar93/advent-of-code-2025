module Day8 (answers) where

import Parser

import Data.List
import Data.Ord

-- | The test input file
testInputFile :: String
testInputFile = "res/Day8_test_input.txt"

-- | The input file
inputFile :: String
inputFile = "res/Day8_input.txt"

-- | Position of a single junction box
type Pos = (Int, Int, Int)

-- | Parses the positions
parsePos :: Parser Pos
parsePos = do
    x <- parseInt
    _ <- parseChar ','
    y <- parseInt
    _ <- parseChar ','
    z <- parseInt
    _ <- parseSpaces
    return (x,y,z)

-- | Reads the input from the specified file
readInput :: String -> IO [Pos]
readInput file = do
    contents <- readFile file
    return $ runParser (some parsePos) contents

-- | Returns the euclideance distance squared between two positions (i.e. junction boxes)
distanceSqr :: Pos -> Pos -> Int
distanceSqr (x0,y0,z0) (x1,y1,z1) = (x1-x0)*(x1-x0) + (y1-y0)*(y1-y0) + (z1-z0)*(z1-z0)

-- | Makes circuits using n connections
mkCircuits :: Int -> [Pos] -> ([[Pos]],[Pos])
mkCircuits nC ps = mkCircuits' 0 distancePairs initialCircuits where
    -- | Our list of pairs (sorted by distance)
    distancePairs = pairDistances ps
    -- | Our initial circuits
    initialCircuits = map (:[]) ps
    -- | Recursive circuit injection
    mkCircuits' _ []      cs = (cs,[])
    mkCircuits' n ((p,o,_):pss) cs | length cs' == 1 = (cs',[p,o])
                                   | n == nC         = (cs,[p,o])
                                   | otherwise       = mkCircuits' n' pss' cs'
                                    where
                                        n'   = n + 1
                                        pss' = pss
                                        cs'  = injectCircuit cs [p,o]
                              
-- | Returns the list of pairs sorted by distance
pairDistances :: [Pos] -> [(Pos, Pos, Int)]
pairDistances ps = sortBy (\(_,_,d0) (_,_,d1) -> d0 `compare` d1) distances where 
    distances = [(x, y, distanceSqr x y) | (x:ys) <- tails ps, y <- ys]

-- | Injects the new circuit (a junction box pair) into the list of circuits, 
-- | merging circuits if necessary
injectCircuit :: [[Pos]] -> [Pos] -> [[Pos]]
injectCircuit circuits newCircuit = case [c | c <- circuits, p <- newCircuit, p `elem` c] of
    -- Brand new circuit 
    []  -> newCircuit:circuits
    -- Found one or more linking circuits => merge
    circuitsToMerge -> mergedCircuit:remainingCircuits where
        mergedCircuit     = (nub . concat) (newCircuit:circuitsToMerge)
        remainingCircuits = circuits \\ circuitsToMerge

-- | Answer to part 1
answer1 :: [Pos] -> Int
answer1 input = product $ take 3 lcircuits where
    lcircuits = sortBy (comparing Down) (map length circuits)
    circuits  = fst $ mkCircuits 1000 input

-- | Answer to part2
answer2 :: [Pos] -> Int
answer2 input = case snd (mkCircuits 99999999999 input) of
    [(x0,_,_), (x1,_,_)] -> x0 * x1
    _                    -> error "No solution"

-- | Answers
answers :: IO [Int]
answers = do
    input <- readInput inputFile
    return [answer1 input, answer2 input]
