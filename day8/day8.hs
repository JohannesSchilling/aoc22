{-# LANGUAGE TypeApplications #-}

import Data.Tuple (swap)
import Data.List (transpose)
import Data.Function (on)
import Debug.Trace (trace)

main :: IO ()
main = do
    input <- map (map (read @Int . (:[]))) . lines <$> getContents
    let dims = (length input, length $ input !! 0)
    putStrLn . (++) "easy: " . show $ solveEasy input dims
    putStrLn . (++) "hard: " . show $ solveHard input dims


solveEasy :: [[Int]] -> (Int, Int) -> Int
solveEasy field (rows, cols) = let
    ixs = [(x, y) | x <- [0..(rows-1)], y <- [0..(cols-1)]]
    -- need to filter out, to not count anything twice
    remaining = filter (not . visibleEastWest field) ixs
    ixs' = map swap remaining
    field' = transpose field
    visNS = length $ filter (visibleEastWest field') ixs'
  in
    (rows * cols) - (length remaining) + visNS


visibleEastWest :: [[Int]] -> (Int, Int) -> Bool
visibleEastWest field (row, col) = let
    (west, (it:east)) = splitAt col $ field !! row
  in
    all (< it) west || all (< it) east

viewDistEastWest :: [[Int]] -> (Int, Int) -> Int
viewDistEastWest field (row, col) = let
    (west, (it:east)) = splitAt col $ field !! row
    distWest = length $ takeWhileInclusive (< it) $ reverse west
    distEast = length $ takeWhileInclusive (< it) east
  in
    distEast * distWest

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive p = foldr (\x ys -> if p x then x:ys else [x]) []

solveHard :: [[Int]] -> (Int, Int) -> Int
solveHard field (rows, cols) = let
    ixs = [(x, y) | x <- [0..(rows-1)], y <- [0..(cols-1)]]
    scoresEW = map (viewDistEastWest field) ixs
    scoresNS = map (viewDistEastWest (transpose field) . swap) ixs
  in
    -- should all be in the same order, so we can multiply things that
    -- are at the same pos
    maximum $ zipWith (*) scoresEW scoresNS
