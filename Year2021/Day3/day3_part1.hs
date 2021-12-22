import Data.List
main = do
    -- Lazily read the file contents
    fileContent <- readFile "day3_input.txt"
    -- Split the file contents by line breaks into a list of strings
    let fileLines = lines fileContent
    -- Create a list of tuples from the list of lines containing each step in numerical format
    let binaryList = map parseLine fileLines
    let halfLength = length binaryList `div` 2
    -- Sum all bits per position resulting in a list of integers
    let bitSum = foldl' (zipWith (+)) (replicate (length (head binaryList)) 0) binaryList
    -- Calculates the most frequent bits per position and converts it to integer
    let mostFrequentInt = bitsToInt $ mostFrequentBits bitSum halfLength
    -- Calculates the least frequent bits per position and converts it to integer
    let leastFrequentInt = bitsToInt $ leastFrequentBits bitSum halfLength
    -- Print the result
    print $ mostFrequentInt * leastFrequentInt


-- Parses the line into an array of zeros and ones
parseLine :: String -> [Int]
-- If starts with 'f' then it is a step forward, so returns a tuple with the first element as the parsed step value and the second element as 0
parseLine = map (read . pure :: Char -> Int)

-- Calculates the most frequent bits per position from bitSum and return the most frequent bits
mostFrequentBits :: [Int] -> Int -> [Int]
mostFrequentBits bitSum threshold = map (\i -> if i < threshold then 0 else 1) bitSum

-- Calculates the least frequent bits per position
leastFrequentBits :: [Int] -> Int -> [Int]
leastFrequentBits bitSum threshold = map (\i -> if i < threshold then 1 else 0) bitSum

-- Converts the bit array to an integer
bitsToInt :: [Int] -> Int
bitsToInt = foldl' (\accumulator element-> element + 2 * accumulator) 0