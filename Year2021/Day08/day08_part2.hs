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
    putStrLn ("inputData: " ++ show inputData)
    let sevenSegmentWiringList = map (words . head) inputData
    let fourDigitDataList = map sort <$> map (words . last) inputData
    putStrLn ("fourDigitDataList: " ++ unlines (map show fourDigitDataList))
    mapResultList <- printEvaluationTime "Time to create maps: " $ map getMapFromSegmentWiring sevenSegmentWiringList
    putStrLn ("mapResultList: " ++ unlines (map show mapResultList))
    digitsList <- printEvaluationTime "Time to decode the digits: " $ zipWith decodeDigits mapResultList fourDigitDataList
    putStrLn ("All digits:\n" ++ unlines (map show digitsList) ++ "============\n")
    putStrLn ("Result: " ++ show (sum digitsList))


getMapFromSegmentWiring :: [String] -> M.Map String Int
getMapFromSegmentWiring wiringList = do
    let sortedWiringList = sortBy (compare `on` length) $ map sort wiringList
    let _mapCreationFold accMap x   | length x == 2 = M.insert 1 x accMap
                                    | length x == 3 = M.insert 7 x accMap
                                    | length x == 4 = M.insert 4 x accMap
                                    | length x == 5 =
                                        if accMap M.! 1 `isSubsequenceOf` x then M.insert 3 x accMap
                                        else if length (x \\ accMap M.! 4) == 3 then M.insert 2 x accMap
                                        else M.insert 5 x accMap
                                    | length x == 6 =
                                        if not $ accMap M.! 1 `isSubsequenceOf` x then M.insert 6 x accMap
                                        else if null (accMap M.! 4 \\ x) then M.insert 9 x accMap
                                        else M.insert 0 x accMap
                                    | otherwise = M.insert 8 x accMap
    let invertedResult = foldl' _mapCreationFold M.empty sortedWiringList
    trace ("<<debug>> wiring: " ++ show sortedWiringList ++ " map: " ++ show invertedResult) M.fromList $ map (\(x,y) -> (y,x)) $ M.toList invertedResult


decodeDigits :: M.Map String Int -> [String] -> Int
decodeDigits xMap digitList = sum $ zipWith (*) [1000,100,10,1] $ map (xMap M.!) digitList