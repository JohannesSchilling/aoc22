{-# LANGUAGE LambdaCase, BlockArguments #-}

import Data.List (transpose)
import Data.List.Split (splitOn)
import Control.Lens ((&), (%~), ix)

main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    let (stacks:instrs:_) = splitOn [""] input
        ss = readStacks stacks
        is = map readInst instrs
    putStrLn $ "easy: " ++ (map head $ evalInsts reverse ss is)
    putStrLn $ "hard: " ++ (map head $ evalInsts id      ss is)

-- i'm kinda proud of this. the magic lies in `transpose` (matrix
-- transpose, i.e. lines to columns), and then just™ taking the right
-- indices of sublists.
readStacks ls = let
    t = transpose ls
    ixs = takeWhile (< length t) [1,5..]
  in
    map (dropWhile (== ' ') . init . (t !!)) ixs

data Inst = Inst Int Int Int
  deriving (Show, Eq)

readInst line = let
    ws = words line
    (count:from:to:_) = map (read . (ws !!)) [1,3,5]
  in
    Inst count (from-1) (to-1)

evalInsts :: ([Char] -> [Char]) -> [[Char]] -> [Inst] -> [[Char]]
evalInsts extraModify stacks [] = stacks
evalInsts extraModify stacks ((Inst count from to):is) = let
    modify :: Int -> ([Char] -> [Char]) -> [[Char]] -> [[Char]]
    modify i f xs = xs & ix i %~ f

    newAdded = modify to ((extraModify . take count $ stacks !! from) ++) stacks
    oldDropped = modify from (drop count) newAdded
  in
    evalInsts extraModify oldDropped is
