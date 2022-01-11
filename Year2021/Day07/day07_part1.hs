import Control.Benchmark
import Data.List.Split
import Debug.Trace
import Data.List


main = do
    -- Lazily read the file contents
    fileContent <- readFile "day07_input.txt"
    -- Split the file contents by commas into a list of strings
    let crabPositions = map (read :: String -> Int) $ splitOn "," fileContent
    -- Run the parsing benchmark
    crabPositions <- printEvaluationTime "Time to parse: " crabPositions
    mostCommonElement <- printEvaluationTime "Most Commom Element time: " $
        head $ maximumBy (\x y -> compare (length x) (length y)) $ group $ sort crabPositions
    totalFuelConsumption <- printEvaluationTime "Calculate fuel time: " $ sum $ map (\x -> abs (x - mostCommonElement)) crabPositions
    putStrLn ("result: " ++ show totalFuelConsumption)
    -- Simulate 80 days for each fish and sum the resulting number
    -- let multisetResult = M.size . (!! 80) . iterate multisetSimulation $ M.fromList crabPositions
    -- -- Run the simulation benchmark
    -- multisetResult <- printEvaluationTime "Simulation time: " multisetResult
    -- -- Print the result
    -- putStrLn ("result: " ++ show multisetResult)

