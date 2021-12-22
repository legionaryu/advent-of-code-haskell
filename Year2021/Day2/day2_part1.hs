main = do
    -- Lazily read the file contents
    fileContent <- readFile "day2_input.txt"
    -- Split the file contents by line breaks into a list of strings
    let fileLines = lines fileContent
    -- Create a list of tuples from the list of lines containing each step in numerical format
    let stepList = map parseLine fileLines
    -- Calculates the finalPosition by adding all positions in the list
    let finalPosition = sumPositions stepList
    -- Multiply the horizontal and vertical final positions to get the final answer
    let result = uncurry (*) finalPosition
    print result

-- Parses the line into a tuple representing a position step
parseLine :: String -> (Int, Int)
-- If starts with 'f' then it is a step forward, so returns a tuple with the first element as the parsed step value and the second element as 0
parseLine ('f':strTail) = ((read . pure :: Char -> Int) (last strTail), 0)
-- If starts with 'd' then it is a step down, so returns a tuple with the first element as 0 and the second element as the parsed step value
parseLine ('d':strTail) = (0, (read . pure :: Char -> Int) (last strTail))
-- If starts with 'u' then it is a step up, so returns a tuple with the first element as 0 and the second element negating the parsed step value
parseLine ('u':strTail) = (0, - (read . pure :: Char -> Int) (last strTail))
-- If doesn't start with 'f', 'd' or 'u' then it is an invalid line, so returns a zero tuple
parseLine _ = (0, 0)

-- Sum a list of position tuples
sumPositions :: [(Int, Int)] -> (Int, Int)
-- If the list is empty, return a zero tuple
sumPositions [] = (0, 0)
-- Otherwise, sum the elements of the list with the result of summing the rest of the list
sumPositions ((x, y):xs) = (x + xsSum, y + ysSum)
    where
        (xsSum, ysSum) = sumPositions xs