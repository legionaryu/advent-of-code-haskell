import qualified Data.Map as M
import Control.Benchmark
import Data.Function
import Data.List
import Data.List.Split
import Debug.Trace


main = do
    -- Lazily read the file contents
    fileContent <- readFile "day08_input.txt"
    -- Split the file contents by commas into a list of strings
    inputData <- printEvaluationTime "Time to parse: " $ map (splitOn " | ") $ lines fileContent
    -- Get the list that represent all possible digits with the current wiring
    let sevenSegmentWiringList = map (words . head) inputData
    -- Get the four digits displayed by the current sensor reading and sort each wiring list by alphabetical order
    let fourDigitDataList = map sort <$> map (words . last) inputData
    -- Create an map from the wiring to be able to parse the four digits
    mapResultList <- printEvaluationTime "Time to create maps: " $ map getMapFromSegmentWiring sevenSegmentWiringList
    -- Parse the four digits
    digitsList <- printEvaluationTime "Time to decode the digits: " $ zipWith decodeDigits mapResultList fourDigitDataList
    putStrLn ("All digits:\n" ++ unlines (map show digitsList) ++ "============\n")
    putStrLn ("Result: " ++ show (sum digitsList))


-- Get a list of sorted wirings and returns a Map able to converts each digit to a number
getMapFromSegmentWiring :: [String] -> M.Map String Int
getMapFromSegmentWiring wiringList = do
    -- Sort by length because the smaller lengths are known digits
    let sortedWiringList = sortBy (compare `on` length) $ map sort wiringList
    -- Create a map with the digit as key and the wiring as value
    let _mapCreationFold accMap x   | length x == 2 = M.insert 1 x accMap -- The digit 1 is the only with two wirings
                                    | length x == 3 = M.insert 7 x accMap -- The digit 7 is the only with three wirings
                                    | length x == 4 = M.insert 4 x accMap -- The digit 4 is the only with four wirings
                                    | length x == 5 = -- There are three possible digits with five wirings
                                        if accMap M.! 1 `isSubsequenceOf` x then M.insert 3 x accMap -- The digit 3 is the only one that contains the digit 1
                                        else if length (x \\ accMap M.! 4) == 3 then M.insert 2 x accMap -- The digit 2 is the only one that has 3 wirings not contained in the digit 4
                                        else M.insert 5 x accMap -- Otherwise the digit 5 is the only one remaining
                                    | length x == 6 = -- There are three possible digits with six wirings
                                        if not $ accMap M.! 1 `isSubsequenceOf` x then M.insert 6 x accMap -- The digit 6 is the only one that contains the digit 1
                                        else if null (accMap M.! 4 \\ x) then M.insert 9 x accMap -- The digit 9 is the only one that overlaps with the digit 4
                                        else M.insert 0 x accMap -- Otherwise the digit 0 is the only one remaining
                                    | otherwise = M.insert 8 x accMap -- The digit 8 is the only with seven wirings
    -- Run the mapping function over the list and return the inverted map
    let invertedResult = foldl' _mapCreationFold M.empty sortedWiringList
    -- Invert the map to make the wring as key and the digit as value
    trace ("<<debug>> wiring: " ++ show sortedWiringList ++ " map: " ++ show invertedResult) M.fromList $ map (\(x,y) -> (y,x)) $ M.toList invertedResult


-- Decode the four digits using the wiring map
decodeDigits :: M.Map String Int -> [String] -> Int
-- Parse each digit and multiply by the corresponding slot then sum all digits to return the full number
decodeDigits xMap digitList = sum $ zipWith (*) [1000,100,10,1] $ map (xMap M.!) digitList