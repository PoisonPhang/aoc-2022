module Day03.Rucksack where

import Data.Char
import Data.List

chunksOf _ [] = []
chunksOf k xs = let (as, bs) = splitAt k xs 
                in as : chunksOf k bs

charToPoint :: Char -> Int
charToPoint c
  | isLower c = (ord c) - (ord 'a') + 1
  | isUpper c = ord c - 38 
  
splitInHalf :: String -> ([Char], [Char])
splitInHalf line = do
  splitAt ((length line) `div` 2) line
  
getMatch :: ([Char], [Char]) -> [Char]
getMatch (one, two) = do
  one `intersect` two

lineToPoints :: String -> Int
lineToPoints line = do
  charToPoint (head $ getMatch (splitInHalf line))
  
groupToPoints :: [String] -> Int
groupToPoints group = do
  charToPoint (head $ foldr (intersect) (head group) (tail group))

countRuckSackPoints :: String -> IO ()
countRuckSackPoints fileName = do
  content <- readFile fileName
  print $ foldr (+) 0 (map lineToPoints (lines content))
  
countGroupPoints :: String -> IO ()
countGroupPoints fileName = do
  content <- readFile fileName
  print $ foldr (+) 0 (map groupToPoints (chunksOf 3 (lines content)))
