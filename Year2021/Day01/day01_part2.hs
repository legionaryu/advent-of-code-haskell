main = do
    -- Lazily read the file contents
    fileContent <- readFile "day01_input.txt"
    -- Split the file contents by line breaks into a list of strings
    let fileLines = lines fileContent
    -- Convert the list of strings into a list of integers
    let intList = map read fileLines :: [Int]
    -- Creates one list offsetting the values by one and another offsetting the values by two
    -- Then zips the original list with the two offset lists and returns a list for the sum of its elements
    let sumList = map (\(x, y, z) -> x + y + z) (zip3 intList (drop 1 intList) (drop 2 intList))
    -- Creates a copy o intList removing the first element
    -- Then compares the intList to the copy of intList putting the resulting boolean in another list
    -- After that removes all elements that are not equal to True and count the number of elements
    let result = length (filter (==True) (zipWith (<) sumList (tail sumList)))
    print result
