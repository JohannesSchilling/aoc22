{-# LANGUAGE ViewPatterns #-}
import Data.List (stripPrefix)
import Data.List.Split (chunksOf)
import Control.Monad (ap)

main :: IO ()
main = do
    input <- lines <$> getContents
    putStrLn $ (++) "one: " $ show . day10'1 $ map read input
    putStrLn $ (++) "two: \n" $ day10'2 $ map read input

data Inst = N
          | A Int
  deriving (Show, Eq)

instance Read Inst where
    readsPrec _ ('n':_) = [(N, "")]
    readsPrec _ (stripPrefix "addx " -> Just v) = [(A $ read v, "")]

sim :: [Inst] -> Int -> [Int] -> [Int]
sim (i:is) x vs = case i of
    N -> sim is x (x:vs)
    (A n) -> sim is (x+n) ((x+n):x:vs)
sim [] _ vs = reverse vs

day10'1 :: [Inst] -> Int
day10'1 is = let
    s = sim is 1 [1]
  in
    sum $ map (ap (*) ((s !!) . subtract 1)) [20,60..220]

day10'2 :: [Inst] -> String
day10'2 is = let
    s = sim is 1 [1]
    crtChar x i = if (abs $ (i `mod` 40) - x) <= 1 then '#' else '.'
    crt = zipWith crtChar s [0..]
  in
    unlines . take 6 . chunksOf 40 $ crt
