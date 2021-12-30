import Data.List
import Debug.Trace

main = do
    -- Lazily read the file contents
    fileContent <- readFile "day03_input.txt"
    -- Split the file contents by line breaks into a list of strings
    let fileLines = lines fileContent
    -- Create a list of tuples from the list of lines containing each step in numerical format
    let binaryList = map parseLine fileLines
    -- Calculates the most frequent bits per position and converts it to integer
    let oxygenRatingInt = bitsToInt $ oxygenRatingFromBitList binaryList
    -- Calculates the least frequent bits per position and converts it to integer
    let carbonRatingInt = bitsToInt $ carbonRatingFromBitSum binaryList
    -- Print the result
    print $ oxygenRatingInt * carbonRatingInt


-- Parses the line into an array of zeros and ones
parseLine :: String -> [Int]
-- Transform every char in the binary string into an integer
parseLine = map (read . pure :: Char -> Int)

-- Calculates the most frequent bits per position from bitSum and return the most frequent bits
oxygenRatingFromBitList :: [[Int]] -> [Int]
oxygenRatingFromBitList bitList
    | length bitList == 1 = head bitList
    | fromIntegral (sum (map head bitList)) >= fromIntegral (length bitList) / 2 = trace "oxygen step result : 1" $ 1 : oxygenRatingFromBitList (shiftList (filterFromFirstBit bitList 1))
    | otherwise = trace "oxygen step result : 1" $ 0 : oxygenRatingFromBitList (shiftList (filterFromFirstBit bitList 0))

-- Calculates the least frequent bits per position
carbonRatingFromBitSum :: [[Int]] -> [Int]
carbonRatingFromBitSum bitList
    | length bitList == 1 = head bitList
    | fromIntegral (sum (map head bitList)) < fromIntegral (length bitList) / 2 = trace "carbon step result : 1" $ 1 : carbonRatingFromBitSum (shiftList (filterFromFirstBit bitList 1))
    | otherwise = trace "carbon step result : 0" $ 0 : carbonRatingFromBitSum (shiftList (filterFromFirstBit bitList 0))

-- Remove the first element of all lists and return the result
shiftList :: [[Int]] -> [[Int]]
shiftList bitList = trace ("+++++++\nbitList:\n" ++ unlines (map show bitList) ++ "result:\n" ++ unlines (map (show . tail) bitList) ++ "+++++++\n") map tail bitList

-- Removes the elements from the list that have the first bit equals the filter parameter
filterFromFirstBit :: [[Int]] -> Int -> [[Int]]
filterFromFirstBit binaryList bit = do
    let filteredValue = filter (\x -> head x == bit) binaryList
    trace ("-------\nfiltered value:\n" ++ unlines (map show filteredValue) ++ "-------\n") filteredValue

-- Converts the bit array to an integer
bitsToInt :: [Int] -> Int
bitsToInt = foldl' (\accumulator element-> element + 2 * accumulator) 0