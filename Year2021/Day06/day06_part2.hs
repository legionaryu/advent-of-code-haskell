import qualified Data.Text as T
import qualified Data.MultiSet as M
import Debug.Trace ( trace )
import Control.Parallel.Strategies (parListChunk, rdeepseq, withStrategy)

main = do
    -- Lazily read the file contents
    fileContent <- readFile "day06_input.txt"
    -- Split the file contents by commas into a list of strings
    let lanternFishes = map (read . T.unpack) (T.splitOn (T.pack ",") (T.pack fileContent))
    -- let chunkSize = max (length lanternFishes `div` 8) 1
    -- let parallelMap = withStrategy (parListChunk chunkSize rdeepseq) . map (runFishSimulation 80)
    -- -- Simulate 256 days for each fish and sum the resulting number
    -- let totalFishCount = sum $ parallelMap lanternFishes
    let totalFishCount = M.size . (!! 256) . iterate multisetSimulation $ M.fromList lanternFishes
    putStrLn ("result: " ++ show totalFishCount)


-- I've thought that this challenge would be a good opportunity to learn about data structures and types and newtypes
-- so I've created a newtype to represent a fish, making it easier to enforce the simulation rules 
newtype LanternFish = LanternFish {
    -- | Amount of days left until the fish creates a new one
    daysToBranch :: Int
} deriving (Show)


-- This is a shortcut to the newtype constructor, effectively creating a new fish with the given number of days to branch
createFishFromNumber :: Int -> LanternFish
createFishFromNumber n = {- trace ("New fish: " ++ show n) -} LanternFish {daysToBranch = n}


runFishSimulation :: Int -> LanternFish -> Int
runFishSimulation days fish | days <= daysToBranch fish = trace ("<<end lte>> days: " ++ show days ++ " | fish: " ++ show fish) 1
                            | otherwise = do
                                let remainingDays = days - 1 - daysToBranch fish
                                let offspringCount = div remainingDays 6
                                let offspring = sum $ map (\x -> runFishSimulation (remainingDays - x * 6) (createFishFromNumber 8)) [0..offspringCount] in
                                    trace ("<<branch>> days: " ++ show days ++ " | fish: " ++ show fish ++ " | remainingDays: " ++
                                    show remainingDays ++ " | offspring: " ++ show offspring) offspring


{- -- Simulates the life cycle of a fish for the given number of days returning the total number of existing fish by the end of the simulation
runFishSimulation :: Int -> LanternFish -> Int
runFishSimulation days fish -- If the fish don't have the days needed to branch, return the 1
                            | daysToBranch fish >= days = {- trace ("<<end>> fish: " ++ show fish ++ " | days: " ++ show days) -} 0
                            -- Otherwise, it means that the fish have branched, so create a new fish with 8 days to branch and resets the current to 6 days
                            -- running the simulation recursively
                            | otherwise = {- trace ("<<branch>> days: " ++ show days ++ " remaining: " ++ show remainingDays) -} 1 + div remainingDays 6 + runFishSimulation remainingDays newFish
                                where
                                    -- To calculate the remainingDays we need to subtract the days spent before branching and, also, subtract the current day
                                    remainingDays = days - daysToBranch fish - 1
                                    -- A new fish needs 8 days to branch
                                    newFish = createFishFromNumber 8 -}


multisetSimulation :: M.MultiSet Int -> M.MultiSet Int
multisetSimulation = M.concatMap (\i -> if i == 0 then [6, 8] else [i - 1])
