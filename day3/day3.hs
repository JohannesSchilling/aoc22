{-# LANGUAGE LambdaCase, BlockArguments #-}

import Data.List (splitAt)
import Data.List.Extra (chunksOf)
import Data.Set (fromList, intersection, elems)
import Data.Function (on)
import GHC.Exts (the)
import Control.Lens (over, both)

main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    putStrLn $ "easy: " ++ (show $ solveEasy input)
    putStrLn $ "hard: " ++ (show $ solveHard input)

solveEasy :: [String] -> Int
solveEasy = solve . map splitInHalf
  where
    splitInHalf xs = let
        n = length xs `div` 2
      in
        [take n xs, drop n xs]

priority :: Char -> Int
priority x | x `elem` ['a'..'z'] = (fromEnum x) - (fromEnum 'a') + 1
           | x `elem` ['A'..'Z'] = (fromEnum x) - (fromEnum 'A') + 1 + 26
priority _ = error "invalid char for priority"

solveHard :: [String] -> Int
solveHard = solve . chunksOf 3

solve :: [[String]] -> Int
solve = sum . map (priority . findCommon')

findCommon' :: [[Char]] -> Char
findCommon' = the . elems . foldl1 intersection . map fromList
