{-# LANGUAGE LambdaCase, BlockArguments #-}

import Data.List (splitAt)
import Data.List.Extra (chunksOf)
import Data.List.Split (splitOn)
import Data.Set (fromList, intersection, elems)
import Data.Function (on)
import GHC.Exts (the)
import Control.Lens (over, both)


type Range = (Int, Int)

main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    putStrLn $ "easy: " ++ (show $ solveEasy input)
    putStrLn $ "hard: " ++ (show $ solveHard input)

readRange :: String -> Range
readRange r = let
    (a:b:_) = map read . (splitOn "-") $ r
  in
    (a, b)

rangeContains :: Range -> Range -> Bool
rangeContains (a, b) (c, d) = (c <= a && b <= d) || (a <= c && d <= b)

solveEasy :: [String] -> Int
solveEasy = solve rangeContains

solveHard :: [String] -> Int
solveHard = solve overlaps

overlaps :: Range -> Range -> Bool
overlaps (a, b) (c, d) = (a <= c && b >= c) || (a <= d && b >= d) || rangeContains (a, b) (c, d)

solve :: (Range -> Range -> Bool) -> [String] -> Int
solve f = sum . map ((\(x:y:_) -> fromEnum $ f x y) . map readRange . splitOn ",")
