import Control.Benchmark
import Data.List
import Data.List.Split
import Debug.Trace
import GHC.Exts (sortWith)
import Util (unzipWith)


main = do
    -- Lazily read the file contents
    fileContent <- readFile "day07_input.txt"
    -- Split the file contents by commas into a list of strings
    let crabPositions = map (read :: String -> Int) $ splitOn "," fileContent
    -- Run the parsing benchmark
    crabPositions <- printEvaluationTime "Time to parse: " crabPositions
    -- Calculate the mean of the positions and store the integral and fractional parts in separate variables
    (truncatedMean, decimalMean) <- printEvaluationTime "Time to calculate the 'mean': " (properFraction (fromIntegral(sum crabPositions) / fromIntegral(length crabPositions)) :: (Int, Float))
    -- We can't use the native 'round' function because it rounds to the nearest integer, not to the nearest integer below
    let mean = if decimalMean > 0.5 then truncatedMean + 1 else truncatedMean
    -- Print the mean position
    putStrLn ("Mean position:" ++ show mean)
    -- Calculates the best crab position and the best fuel consumption based on the input
    totalFuel <- printEvaluationTime "Time to calculate 'fuel consumption': " $ fuelConsumption mean crabPositions
    putStrLn ("Total fuel: " ++ show totalFuel)


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


-- The triangular numbers are the sum of the first x natural numbers 
-- https://en.wikipedia.org/wiki/Triangular_number
triangularNumber :: Integral a => a -> a
triangularNumber x = let xAbs = abs x in xAbs * (xAbs + 1) `div` 2


-- Calculates the total fuel consumption of a crab position list for a given position
fuelConsumption :: Integral a => a -> [a] -> a
fuelConsumption target positions = sum $ map (\x -> triangularNumber (x - target)) positions