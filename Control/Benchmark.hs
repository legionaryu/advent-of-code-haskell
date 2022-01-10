module Control.Benchmark (printEvaluationTime) where

import Control.DeepSeq
import Data.Time.Clock

printEvaluationTime :: NFData b => [Char] -> b -> IO b
printEvaluationTime str x = do
    start <- getCurrentTime
    end <- deepseq x getCurrentTime
    putStrLn (str ++ show (diffUTCTime end start))
    return x