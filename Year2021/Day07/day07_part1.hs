import Control.Benchmark
import Data.List.Split
import Debug.Trace
import Data.List


main = do
    -- Lazily read the file contents
    fileContent <- readFile "day07_input.txt"
    -- Split the file contents by commas into a list of strings
    let crabPositions = map (read :: String -> Int) $ splitOn "," fileContent
    -- Run the parsing benchmark
    crabPositions <- printEvaluationTime "Time to parse: " crabPositions
    crabMedian <- printEvaluationTime "Calculate median time: " $ median crabPositions
    totalFuelConsumption <- printEvaluationTime "Calculate fuel time: " $ sum $ map (\x -> abs (x - crabMedian)) crabPositions
    putStrLn ("result: " ++ show totalFuelConsumption)


-- Calculates de median of an Integral list
median :: Integral a => [a] -> a
median  x = let xLength = length x
                -- The list need to be sorted to find the median
                xSorted = sort x in
                if odd xLength
                    -- If the list has an odd number of elements, the median is the middle element
                    then xSorted !! (xLength `div` 2)
                    -- Else we need to calculate the mean of the two middle elements
                    else (xSorted !! (xLength `div` 2 - 1) + xSorted !! (xLength `div` 2)) `div` 2
