import Data.List
import Debug.Trace
import Data.Maybe

main = do
    -- Lazily read the file contents
    fileContent <- readFile "day4_input.txt"
    -- Split the file contents by line breaks into a list of strings
    let fileLines = lines fileContent
    -- Create a list integers representing the bingo numbers drawn in sequence
    let lotteryIntList = parseLottery $ head fileLines
    -- Create a list of a tuple of bingo cards from the input and the total sum of the bingo card numbers
    let bingoCardsList = map parseBingoCard (filterNotEmpty $ groupNonEmpty $ tail fileLines)
    -- Run the loop drawing the numbers and checking if there is a winner, as soon as a winner is found calculate the score
    let score = runBingoLottery 0 lotteryIntList bingoCardsList
    print score


-- Check if both arrays are empty
groupNonEmpty :: [[a]] -> [[[a]]]
groupNonEmpty = groupBy (\leftElement rightElement -> not (null leftElement) && not (null rightElement))


-- Filter not null inner lists
filterNotEmpty :: [[[a]]] -> [[[a]]]
filterNotEmpty = filter (not . null . head)


-- Parses the line by splitting the string at the ',' (commas) and converting the resulting string array into an integer array
parseLottery :: String -> [Int]
parseLottery str = map (read :: String -> Int) $ splitBy ',' str;


-- Splits a string into a list of strings separated by the delimiter (origin https://stackoverflow.com/a/7569301)
splitBy :: Char -> String -> [String]
splitBy _ "" = [];
splitBy delimiterChar inputString = foldr (splitStep delimiterChar) [""] inputString


-- Step for the splitBy function
splitStep :: Char -> Char -> [String] -> [String]
splitStep _ _ [] = []
splitStep delimiterChar currentChar allStrings@(partialString:handledStrings)
    | currentChar == delimiterChar = "":allStrings -- start a new partial string at the head of the list of all strings
    | otherwise = (currentChar:partialString):handledStrings -- add the current char to the partial string


-- Parses a list of strings into a list of string numbers, then converts them to integers
-- after that, creates a tuple containing the total sum of the bingo card numbers and the bingo card itself
parseBingoCard :: [String] -> (Int, [[Int]])
parseBingoCard strList = do
    -- Splits the string at the spaces
    let bingoCardStr = map words strList
    -- Converts the string numbers to integers
    let bingoCard = map (map (read :: String -> Int)) bingoCardStr
    -- Calculates the total sum of the bingo card numbers
    let bingoCardSum = sum $ map sum bingoCard
    -- Returns the tuple of the total sum and the bingo card
    (bingoCardSum, bingoCard)


-- Replace the drawnNumber with minus one (-1) if it exists in the card and return a tuple with a True and the new card, False and the old card otherwise
replaceDrawNumber :: Int -> (Int, [[Int]]) -> (Bool, [[Int]])
replaceDrawNumber drawnNumber bingoCardTuple = do
    -- Decouple the total sum and the card
    let (oldSum, bingoCard) = bingoCardTuple
    -- Check if the drawn number exists in the card
    let foundNumberOnCard = any (elem drawnNumber) bingoCard
    if foundNumberOnCard then
        -- Replace all drawn number occurrences with minus one (-1)
        let newBingoCard = map (map (\x -> if x == drawnNumber then -1 else x)) bingoCard in
            (True, newBingoCard)
    else
        -- If the drawn number does not exist in the card, return the old card
        (False, bingoCard)


-- Runs the bingo lottery recursively by dequeuing the first number from the lottery list and replacing it with minus one (-1) if it exists in the card
runBingoLottery :: Int -> [Int] -> [(Int, [[Int]])] -> Int
runBingoLottery step lotteryNumbers cardList = do
    -- Get the first lottery number from the list
    let drawnNumber = head lotteryNumbers
    -- Updates total sum + card tuple with the drawn number
    let _updateBingoCards (cardSum, card) = do
        -- Replace the drawnNumber with minus one (-1) if it exists in the card
        let (bingoCardMatches, newBingoCard) = replaceDrawNumber drawnNumber (cardSum, card)
        -- If the drawn number exists in the card, return the new card and the new total sum
        if bingoCardMatches then
            (cardSum - drawnNumber, newBingoCard)
        else -- Else return the old card and the old total sum
            (cardSum, card)
    -- Update all bingo cards
    let updatedBingoCards = map _updateBingoCards cardList
    -- The condition to be a winner card is fill one row or column with minus one (-1)
    -- hence the sum of a filled row or column is minus five (-5)
    let _winnerMatcher (cardSum, card) = any ((== -5) . sum) card || any ((== -5) . sum) (transpose card)
    -- Check if there is a winner card
    let winners = filter _winnerMatcher updatedBingoCards
    -- If there is no winner, keep running the lottery
    if null winners then
        trace ("step: " ++ show step) runBingoLottery (step + 1) (tail lotteryNumbers) updatedBingoCards
    else -- Else return the total sum of the winner card multiplied by the last drawn number
        trace ("final step: " ++ show step) drawnNumber * fst (head winners)
