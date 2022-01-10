import qualified Data.MultiSet as M
import Control.Benchmark
import Data.List.Split
import Debug.Trace


main = do
    -- Lazily read the file contents
    fileContent <- readFile "day06_input.txt"
    -- Split the file contents by commas into a list of strings
    let lanternFishes = map (read :: String -> Int) $ splitOn "," fileContent
    -- Run the parsing benchmark
    lanternFishes <- printEvaluationTime "Time to parse: " lanternFishes
    -- Simulate 256 days for each fish and sum the resulting number
    let multisetResult = M.size . (!! 256) . iterate multisetSimulation $ M.fromList lanternFishes
    -- Run the simulation benchmark
    multisetResult <- printEvaluationTime "Simulation time: " multisetResult
    -- Print the result
    putStrLn ("result: " ++ show multisetResult)


-- | Simulate the growth of a fish population for a given number of days, by replacing the itens of the MultiSet
-- with the reset fish and the new fish when it's internal counter reaches 0.
multisetSimulation :: M.MultiSet Int -> M.MultiSet Int
multisetSimulation = M.concatMap (\i -> if i == 0 then [6, 8] else [i - 1])
