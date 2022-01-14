import Control.Benchmark
import Data.List.Split


main = do
    -- Lazily read the file contents
    fileContent <- readFile "day08_input.txt"
    -- Split the file contents by commas into a list of strings
    let sevenSegmentsData = map (splitOn " | ") $ lines fileContent
    let fourDigitData = concatMap (words . last) sevenSegmentsData
    let onUniqueNumberDigits x  | xLength == 2 = True
                                | xLength == 3 = True
                                | xLength == 4 = True
                                | xLength == 7 = True
                                | otherwise = False where xLength = length x
    let uniqueNumberDigits = filter onUniqueNumberDigits fourDigitData
    putStrLn ("Result: " ++ show (length uniqueNumberDigits))

