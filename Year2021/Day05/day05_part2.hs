import qualified Data.Text as T
import Debug.Trace
import Data.List
import Data.Map.Strict (fromListWith, toList)


main = do
    -- Lazily read the file contents
    fileContent <- readFile "day05_input.txt"
    -- Split the file contents by line breaks into a list of strings
    let fileLines = lines fileContent
    -- Create a list of nested integer tuple representing lines of vents
    let ventTupleList = parseVentLines fileLines
    -- Create a grid containing all the lines and the amount of overlap for each coordinate
    let grid = fillGrid ventTupleList
    let overlappedLines = filter (\(_,i) -> i > 1) grid
    print $ {-trace ("overlapped lines: " ++ show overlappedLines)-} length overlappedLines


-- Parses the lines by splitting it by spaces then converting the first and last elements to integer tuples
parseVentLines :: [String] -> [((Int, Int), (Int, Int))]
parseVentLines lines = do
    -- Apply the function to each line
    map _parseVentLine lines where 
        _parseVentLine line = do
            -- Split the line by spaces
            let lineSplit = words line
            -- Get the first and last elements of the line split
            let firstCoord = head lineSplit
            let lastCoord = last lineSplit
            -- Split the coordinates by commas (using Data.Text package) and convert to integers
            let (x:y:_) = map (read . T.unpack) (T.splitOn (T.pack ",") (T.pack firstCoord))
            let (x2:y2:_) = map (read . T.unpack) (T.splitOn (T.pack ",") (T.pack lastCoord))
            -- Return a tuple of the first and last coordinates
            ((x, y), (x2, y2))


-- Fills the grid with the lines and the amount of overlap for each coordinate
fillGrid :: [((Int, Int), (Int, Int))] -> [((Int, Int), Int)]
fillGrid ventTupleList = do
    -- Create a list of all the line coordinates in the grid
    let nestedCoordList = map createLineFromTuple ventTupleList
    -- Flatten the nested line lists
    let concatenatedList = concat nestedCoordList
    -- Create a map of the coordinates and the amount of times they appear in the grid
    -- by turning the list into a map and casting it back to list
    toList $ fromListWith (+) concatenatedList


-- Creates a list from a range of integers independent of the direction of the line
range :: Integral a => a -> a -> [a]
range start end = do
    -- Save the distance between the start and end
    let distance = end - start
    -- If the distance is negative, reverse the direction of the range, otherwise, keep it the same
    let step = if distance < 0 then -1 else 1
    -- Create a list of the range
    takeWhile (/= (end + step)) (iterate (+ step) start)


-- Creates a list of coordinates from the line coordinates
createLineFromTuple :: ((Int, Int), (Int, Int)) -> [((Int, Int), Int)]
createLineFromTuple ventTuple = do
    -- Uncurry the tuple
    let ((x1, y1), (x2, y2)) = {-trace ("tuple: " ++ show ventTuple)-} ventTuple
    -- If the line is a point return a list with the point
    if x1 == x2 && y1 == y2 then
        [((x1, y1), 1)]
    else do
        -- If the line is horizontal, create a list of coordinates from the range of the x values
        let xList = if x1 == x2 then repeat x1 else range x1 x2
        -- If the line is vertical, create a list of coordinates from the range of the y values
        let yList = if y1 == y2 then repeat y1 else range y1 y2
        -- Zip the x and y lists together and return the list of coordinates
        zipWith (\x y -> ((x, y), 1)) xList yList