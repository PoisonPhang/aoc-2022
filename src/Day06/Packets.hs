module Day06.Packets where

import Data.List

-- Change 14 to 4 for part 1

getSlice :: Int -> Int -> [Char] -> [Char]
getSlice cur size list = do
  drop (cur - size) $ take cur list
  
isNotSet :: [Char] -> Bool
isNotSet list = nub list /= list

findStart :: [Char] -> Int -> Int
findStart list start
  | isNotSet $ getSlice start 14 list = findStart list (start + 1)
  | otherwise                        = start

parseInput :: String -> IO ()
parseInput fileName = do
  content <- readFile fileName
  print $ findStart content 14