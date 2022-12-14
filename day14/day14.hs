{-# LANGUAGE TupleSections #-}

import Prelude hiding (drop, floor)

import Data.List.Split (splitOn, chunksOf)
import Data.List (foldl', maximumBy)
import qualified Data.Map.Strict as M
import Data.Function (on)

main :: IO ()
main = do
    input <- lines <$> getContents
    let traces = concatMap expandTrace $ map parseTrace input
    putStrLn . (++) "day14'1: " . show $ day14'1 traces
    putStrLn . (++) "day14'2: " . show $ day14'2 traces

parseTrace :: String -> [(Int, Int)]
parseTrace = map (read . ('(':) . (++ ")")) . splitOn " -> "

data Tile = Rock | Sand | Air
  deriving (Show, Eq)

expandTrace :: [(Int, Int)] -> [(Int, Int)]
expandTrace trace = concat . zipWith expandEdge trace $ tail trace
  where
    expandEdge (ax, ay) (bx, by) | ax == bx = map (ax,) $ [ay..by] ++ [by..ay]
    expandEdge (ax, ay) (bx, by) | ay == by = map (,by) $ [ax..bx] ++ [bx..ax]

initialWorld :: [(Int, Int)] -> M.Map (Int, Int) Tile
initialWorld = foldl' (\m pos -> M.insert pos Rock m) M.empty


data Dropped = At (Int, Int) | Abyss (Int, Int)
  deriving (Show, Eq)

drop :: M.Map (Int, Int) Tile -> Int -> (Int, Int) -> Dropped
drop _ abyss pos | (snd pos) > abyss = Abyss pos
drop world abyss pos = case (at lb, at below, at rb) of
    -- using evaluation/inspection order of the cases here <3
    (_, Air, _) -> drop world abyss below
    (Air, _, _) -> drop world abyss lb
    (_, _, Air) -> drop world abyss rb
    otherwise -> At pos
  where
    at :: (Int, Int) -> Tile
    at pos  = M.findWithDefault Air pos world
    below = pos +! (0, 1)
    lb = pos +! (-1, 1)
    rb = pos +! (1, 1)
    (a, b) +! (c, d) = (a+c, b+d)

day14'1 :: [(Int, Int)] -> Int
day14'1 traces = go world 0
  where
    abyss = maximum $ map snd traces
    world = initialWorld traces
    go :: M.Map (Int, Int) Tile -> Int -> Int
    go world count = case drop world abyss (500, 0) of
        At pos -> go (M.insert pos Sand world) (succ count)
        Abyss _ -> count

day14'2 :: [(Int, Int)] -> Int
day14'2 traces = go world 0
  where
    floor = (+) 2 $ maximum $ map snd traces
    floorTrace = map (,floor) [(-1000)..2000]
    world = initialWorld $ traces ++ floorTrace
    go :: M.Map (Int, Int) Tile -> Int -> Int
    go world count = case drop world (floor+42) (500, 0) of
        At (500, 0) -> (succ count)
        At pos -> go (M.insert pos Sand world) (succ count)
        Abyss pos -> error $ "fell beside the world at " ++ (show pos)
