module Common (evaluateAndPrint) where

import Data.Time

-- | Evaluates and prints the result of the computation
evaluateAndPrint :: (Show a) => String -> IO a -> IO ()
evaluateAndPrint s m = do
    startTime <- getCurrentTime
    answer <- m
    endTime <- getCurrentTime
    let time = diffUTCTime endTime startTime
    putStrLn $ s ++ " : " ++ show answer ++ " ; took " ++ show time ++ " to execute"
