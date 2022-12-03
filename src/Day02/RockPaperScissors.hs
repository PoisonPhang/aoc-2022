module Day02.RockPaperScissors where

data Hand
 = Rock
 | Paper
 | Scissors

handToPoint :: Hand -> Int
handToPoint Rock     = 1
handToPoint Paper    = 2
handToPoint Scissors = 3
  
parseHand :: Char -> Hand
parseHand 'A' = Rock
parseHand 'B' = Paper
parseHand 'C' = Scissors
parseHand 'X' = Rock
parseHand 'Y' = Paper
parseHand 'Z' = Scissors

data RoundResult
 = Lost
 | Draw
 | Won

roundResultToPoint :: RoundResult -> Int
roundResultToPoint Lost = 0
roundResultToPoint Draw = 3
roundResultToPoint Won  = 6

parseRoundResult :: Char -> RoundResult
parseRoundResult 'X' = Lost
parseRoundResult 'Y' = Draw
parseRoundResult 'Z' = Won

handPairToResult :: Hand -> Hand -> RoundResult
handPairToResult Rock     Rock     = Draw
handPairToResult Rock     Paper    = Won
handPairToResult Rock     Scissors = Lost
handPairToResult Paper    Rock     = Lost
handPairToResult Paper    Paper    = Draw
handPairToResult Paper    Scissors = Won
handPairToResult Scissors Rock     = Won
handPairToResult Scissors Paper    = Lost
handPairToResult Scissors Scissors = Draw

handAndResultToHand :: Hand -> RoundResult -> Hand
handAndResultToHand Rock     Lost = Scissors
handAndResultToHand Rock     Draw = Rock
handAndResultToHand Rock     Won  = Paper
handAndResultToHand Paper    Lost = Rock
handAndResultToHand Paper    Draw = Paper
handAndResultToHand Paper    Won  = Scissors
handAndResultToHand Scissors Lost = Paper
handAndResultToHand Scissors Draw = Scissors
handAndResultToHand Scissors Won  = Rock

parseLinePart1 :: String -> Int
parseLinePart1 (o : _ : u : []) = do 
  roundResultToPoint (handPairToResult (parseHand o) (parseHand u)) + (handToPoint (parseHand u))

calculateScorePart1 :: String -> IO ()
calculateScorePart1 fileName = do
  text <- readFile fileName
  print $ foldr (+) 0 (map parseLinePart1 (lines text))

parseLinePart2 :: String -> Int
parseLinePart2 (o : _ : u : []) = do 
  handToPoint (handAndResultToHand (parseHand o) (parseRoundResult u)) + (roundResultToPoint (parseRoundResult u))
  
calculateScorePart2 :: String -> IO ()
calculateScorePart2 fileName = do
  text <- readFile fileName
  print $ foldr (+) 0 (map parseLinePart2 (lines text))

