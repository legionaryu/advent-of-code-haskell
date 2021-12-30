main = do
    -- Lazily read the file contents
    fileContent <- readFile "day01_input.txt"
    -- Split the file contents by line breaks into a list of strings
    let fileLines = lines fileContent
    -- Convert the list of strings into a list of integers
    let intList = map read fileLines :: [Int]
    -- Creates a copy o intList removing the first element
    -- Then compares the intList to the copy of intList putting the resulting boolean in another list
    -- After that removes all elements that are not equal to True and count the number of elements
    let result = length (filter (==True) (zipWith (<) intList (tail intList)))
    print result
