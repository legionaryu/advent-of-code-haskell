{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
import qualified Data.Text as T
import qualified Data.MultiSet as M
import Control.DeepSeq
import Data.Time.Clock
import Debug.Trace
import GHC.Generics


main = do
    -- Lazily read the file contents
    fileContent <- readFile "day06_input.txt"
    -- Split the file contents by commas into a list of strings
    let lanternFishes = map (createFishFromNumber . read . T.unpack) (T.splitOn (T.pack ",") (T.pack fileContent))
    lanternFishes <- printEvaluationTime "Parse time: " lanternFishes

    let lanternFishes' = map ((read :: String -> Int) . T.unpack) (T.splitOn (T.pack ",") (T.pack fileContent))
    -- putStrLn ("Parse time: " ++ show (nominalDiffTimeToSeconds parseTime) ++ " seconds")
    -- Simulate 80 days for each fish and sum the resulting number
    let totalFishCount = sum $ map (runFishSimulation 80) lanternFishes
    totalFishCount <- printEvaluationTime "Simulation pure recursive time: " [totalFishCount]

    let lazySimulation = iterate (concatMap runFishSimulation1) lanternFishes'
    simulationResult <- printEvaluationTime "Simulation native list time: " [length $ lazySimulation !! 80]

    let lessRecursionSimulation = sum $ map (runFishSimulation2 80) lanternFishes
    lessRecursionSimulation <- printEvaluationTime "Simulation alternative recursive time: " [lessRecursionSimulation]

    let simulation3Result = sum $ map (runFishSimulation3 80) lanternFishes
    simulation3Result <- printEvaluationTime "Simulation alternative recursive 2 time: " [simulation3Result]

    let multisetResult = M.size . (!! 80) . iterate multisetSimulation $ M.fromList lanternFishes'
    multisetResult <- printEvaluationTime "Simulation multiSet time: " [multisetResult]

    putStrLn ("result: " ++ show totalFishCount)
    -- putStrLn ("result list: " ++ show simulationResult)
    -- putStrLn ("result less recursion: " ++ show lessRecursionSimulation)
    -- putStrLn ("result simulation 3: " ++ show simulation3Result)
    putStrLn ("result multisetResult: " ++ show multisetResult)


printEvaluationTime :: NFData b => [Char] -> b -> IO b
printEvaluationTime str x = do
    start <- getCurrentTime
    end <- deepseq x getCurrentTime
    putStrLn (str ++ show (diffUTCTime end start))
    return x


-- I've thought that this challenge would be a good opportunity to learn about data structures and types and newtypes
-- so I've created a newtype to represent a fish, making it easier to enforce the simulation rules 
newtype LanternFish = LanternFish {
    -- | Amount of days left until the fish creates a new one
    daysToBranch :: Int
} deriving (Show, Generic, NFData)

-- This is a shortcut to the newtype constructor, effectively creating a new fish with the given number of days to branch
createFishFromNumber :: Int -> LanternFish
createFishFromNumber n = {- trace ("New fish: " ++ show n) -} LanternFish {daysToBranch = n}

-- Simulates the life cycle of a fish for the given number of days returning the total number of existing fish by the end of the simulation
runFishSimulation :: Int -> LanternFish -> Int
runFishSimulation days fish -- If the fish don't have the days needed to branch, return the 1
                            | daysToBranch fish >= days = {- trace ("<<end>> fish: " ++ show fish ++ " | days: " ++ show days) -} 1
                            -- Otherwise, it means that the fish have branched, so create a new fish with 8 days to branch and resets the current to 6 days
                            -- running the simulation recursively
                            | otherwise = {- trace ("<<branch>> days: " ++ show days ++ " remaining: " ++ show remainingDays) -} runFishSimulation remainingDays resetFish + runFishSimulation remainingDays newFish
                                where
                                    -- To calculate the remainingDays we need to subtract the days spent before branching and, also, subtract the current day
                                    remainingDays = days - daysToBranch fish - 1
                                    -- A new fish needs 8 days to branch
                                    newFish = createFishFromNumber 8
                                    -- The current fish is reset to 6 days to branch
                                    resetFish = createFishFromNumber 6


runFishSimulation1 :: (Ord a, Num a) => a -> [a]
runFishSimulation1 x = if x > 0 then [x-1] else [6, 8]

runFishSimulation2 :: Int -> LanternFish -> Int
runFishSimulation2 days fish = let remainingDays = days - 1 - daysToBranch fish
                                   daysToReset = 6 + 1
                                   offspringCount = 1 + div remainingDays daysToReset
                                   offspringSimulation = sum $ map (\x -> runFishSimulation2 (remainingDays - (x * daysToReset)) $ createFishFromNumber 8) [0..offspringCount-1] in
                                   if remainingDays < 0 then {- trace ("<<end>> fish: " ++ show fish) -} 1
                                   else {- trace ("<<branch>> fish: " ++ show fish ++ " | offspringCount: " ++ show offspringCount) -} 1 + offspringSimulation


runFishSimulation3 :: Int -> LanternFish -> Int
runFishSimulation3 days fish = do
    let remainingDays = days - 1 - daysToBranch fish
    let daysToReset = 6 + 1
    let offspringCount = 1 + div remainingDays daysToReset
    let _simulateOffspring x = do
        let offspringDays = remainingDays - (x * daysToReset)
        if offspringDays > 8 then runFishSimulation3 offspringDays $ createFishFromNumber 8 else 1
    let offspringSimulation = sum $ map _simulateOffspring [0..offspringCount-1]
    if remainingDays < 0 then {- trace ("<<end>> fish: " ++ show fish) -} 1
    else {- trace ("<<branch>> fish: " ++ show fish ++ " | offspringCount: " ++ show offspringCount) -} 1 + offspringSimulation


multisetSimulation :: M.MultiSet Int -> M.MultiSet Int
multisetSimulation = M.concatMap (\i -> if i == 0 then [6, 8] else [i - 1])
