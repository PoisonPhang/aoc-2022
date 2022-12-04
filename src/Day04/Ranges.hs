module Day04.Ranges where

import Data.List
import qualified Data.Text as Text

createRange :: [String] -> [Int]
createRange nums = [(read (head nums)::Int) .. (read (last nums)::Int)]

rangesOverlap :: [[Int]] -> Bool
rangesOverlap ranges
  | (head ranges) `isInfixOf` (last ranges) = True
  | (last ranges) `isInfixOf` (head ranges) = True
  | otherwise                               = False
  
rangesOverlapAtAll :: [[Int]] -> Bool
rangesOverlapAtAll ranges
  | length ((head ranges) `intersect` (last ranges)) > 0 = True
  | otherwise                               = False
  
parseRange :: String -> [Int]
parseRange range = do
  createRange $ map Text.unpack (Text.splitOn (Text.pack "-") (Text.pack range))

parseLine :: String -> [[Int]]
parseLine line = do
  map parseRange (map Text.unpack (Text.splitOn (Text.pack ",") (Text.pack line)))

countOverlap :: String -> IO ()
countOverlap fileName = do
  content <- readFile fileName
  print $ sum (map fromEnum (map rangesOverlap (map parseLine (lines content))))
  
countOverlapAtAll :: String -> IO ()
countOverlapAtAll fileName = do
  content <- readFile fileName
  print $ sum (map fromEnum (map rangesOverlapAtAll (map parseLine (lines content))))