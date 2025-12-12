module Day9 (answers) where

import Parser

import Data.List
import Data.Ord
import Data.Function

-- | The test input file
testInputFile :: String
testInputFile = "res/Day9_test_input.txt"

-- | The input file
inputFile :: String
inputFile = "res/Day9_input.txt"

-- | A tile pos
type Pos = (Int, Int)

-- | Parses tiles
parseTiles :: Parser [Pos]
parseTiles = some parseTile where
    parseTile = do
        x <- parseInt
        _ <- parseChar ','
        y <- parseInt
        _ <- parseSpaces
        return (x,y)

-- | Reads the input from the specified file
readInput :: String -> IO [Pos]
readInput file = do
    contents <- readFile file
    return $ runParser parseTiles contents

-- | Returns the area of the rectangle
rectangleArea :: Pos -> Pos -> Int
rectangleArea (x0,y0) (x1,y1) = (abs (x0 - x1) + 1) * (abs (y0 - y1) + 1)

-- | Returns all possible rectangle areas in descending order
rectangleSizes :: [Pos] -> [((Pos,Pos),Int)]
rectangleSizes ps = sortBy (flip compare `on` snd) sizes where 
    sizes = [((x, y), rectangleArea x y) | (x:ys) <- tails ps, y <- ys]

-- | Wind triangle 
-- Counterclockwise => out-rectangle
-- Clockwise => in-rectangle
mkRectangles :: [Pos] -> [(Pos, Pos)]
mkRectangles ps = mkRectangles' 0 [] where
    mkRectangles' i valids = if i == noofPos - 1
        then valids'
        else mkRectangles' (i+1) valids'
        where
            noofPos    = length ps
            p0@(x0,y0) = ps !! ((i + noofPos - 1) `mod` noofPos)
            p1@(x1,y1) = ps !! ((i + noofPos + 0) `mod` noofPos)
            p2@(x2,y2) = ps !! ((i + noofPos + 1) `mod` noofPos)
            p3@(x3,y3) = (x1+v1x+v2x,y1+v1y+v2y)
            (v1x, v1y) = (x0-x1,y0-y1)
            (v2x, v2y) = (x2-x1,y2-y1)
            k          = signum $ v1x*v2y - v1y*v2x
            valids'    = if (k < 0) && (p3 `isInside` ps) then (p0,p2):valids else valids

-- | Returns whether the point is on or inside the perimeter
isInside :: Pos -> [Pos] -> Bool 
isInside (x,y) ps = False -- TODO

-- | The answer to part 1
answer1 :: [Pos] -> Int
answer1 = snd . head . rectangleSizes

-- | The answer to part 2
answer2 :: [Pos] -> Int
answer2 input = error $ "Valids : " ++ show valids ++ "\nSizes : " ++ show sizes where --head sortedSizes where
    sortedSizes = sortBy (comparing Down) sizes
    sizes       = map (uncurry rectangleArea) valids 
    valids      = mkRectangles input

-- | The answers
answers :: IO [Int]
answers = do
    input <- readInput testInputFile
    return [answer1 input, answer2 input]
