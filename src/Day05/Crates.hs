module Day05.Crates where

import Data.List
import Data.Char (isSpace)

chunksOf _ [] = []
chunksOf k xs = let (as, bs) = splitAt k xs 
                in as : chunksOf k bs


replaceAtIndex :: Int -> a -> [a] -> [a]    
replaceAtIndex i x xs = take i xs ++ [x] ++ drop (i+1) xs
                
data Action =
  Action {
    move :: Int,
    from :: Int,
    to   :: Int
  }
  deriving (Eq, Show)
  
getActionParts :: [String] -> Action
getActionParts ( _ : move : _ : from : _ : to : []) =
  Action (read move::Int) (read from::Int) (read to::Int)

parseActionLine :: String -> Action
parseActionLine line = do
  getActionParts (words line)

getCrateLable :: String -> Char
getCrateLable ( _ : lable : _ : [])     = lable
getCrateLable ( _ : lable : _ : _ : []) = lable

parseCrateLine :: String -> [Char]
parseCrateLine line = do
  map getCrateLable (chunksOf 4 line)

getCrates :: [String] -> [[Char]]
getCrates crateLines = map (dropWhile isSpace) (transpose $ map parseCrateLine crateLines)

grabCrates :: Int -> [Char] -> ([Char], [Char])
grabCrates n stack = do
  (take n stack, drop n stack)

dropCrates :: [Char] -> [Char] -> [Char]
dropCrates crates stack = do
  -- NOTE: Swap lines for part one
  -- (reverse crates ++ stack)
  crates ++ stack

applyAction :: Action -> [[Char]] -> [[Char]]
applyAction action stacks = do
  let grabRes = grabCrates (move action) (stacks !! ((from action) - 1))
  let dropRes = dropCrates (fst grabRes) (stacks !! ((to action) - 1))
  replaceAtIndex ((from action) - 1) (snd grabRes) (replaceAtIndex ((to action) - 1) dropRes stacks)

applyActions :: [Action] -> [[Char]] -> [[Char]]
applyActions actions stacks = do
  foldr (applyAction) stacks (reverse actions)

parseFile :: String -> IO ()
parseFile fileName = do
  content <- readFile fileName
  let crates = getCrates (takeWhile ("[" `isPrefixOf`) (lines content))
  let actions = map parseActionLine (filter ("m" `isPrefixOf`) (lines content))
  print crates
  print actions
  print $ applyActions actions crates
