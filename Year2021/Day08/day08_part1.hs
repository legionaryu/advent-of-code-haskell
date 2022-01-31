import Control.Benchmark
import Data.List.Split


main = do
    -- Lazily read the file contents
    fileContent <- readFile "day08_input.txt"
    -- Split the file contents by commas into a list of strings
    let sevenSegmentsData = map (splitOn " | ") $ lines fileContent
    -- Get only the four digit representation and flatten it into a list
    let fourDigitData = concatMap (words . last) sevenSegmentsData
    let onUniqueNumberDigits x  | xLength == 2 = True
                                | xLength == 3 = True
                                | xLength == 4 = True
                                | xLength == 7 = True
                                | otherwise = False where xLength = length x
    -- Get only the digits with an unique wiring representation
    let uniqueNumberDigits = filter onUniqueNumberDigits fourDigitData
    -- Show the total number of unique digits
    putStrLn ("Result: " ++ show (length uniqueNumberDigits))

