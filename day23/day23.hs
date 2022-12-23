{-# LANGUAGE TupleSections #-}

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.Maybe (catMaybes, maybeToList, fromJust)
import Data.Maybe.HT (toMaybe)
import Data.List.HT (rotate)
import Data.List (findIndex)
import Debug.Trace (trace)

main :: IO ()
main = do
    input <- elfCoords <$> getContents
    putStrLn $ (++) "day23'1: " $ show $ day23'1 input
    putStrLn $ (++) "day23'2: " $ show $ day23'2 input

type Pos = (Int, Int)

elfCoords :: String -> S.Set Pos
elfCoords = S.fromList . concatMap indexLine . zip [0..] . lines
  where
    indexLine (ix, l) = map ((, ix) . fst) $ filter ((== '#') . snd) $ zip [0..] l

(a, b) +! (c, d) = (a + c, b + d)

proposeMove :: S.Set Pos -> [Pos] -> M.Map Pos [Pos]
proposeMove cs deltas = S.foldl' proposeOne M.empty cs
  where
    proposeOne :: M.Map Pos [Pos] -> Pos -> M.Map Pos [Pos]
    proposeOne m p | allFreeAround p = M.insertWith (++) p [p] m
    proposeOne m p = case catMaybes $ map (free p) deltas of
        [] -> M.insertWith (++) p [p] m
        (p':_) -> M.insertWith (++) p' [p] m
    free :: Pos -> Pos -> Maybe Pos
    free pos delta = toMaybe (all (flip S.notMember cs) pos') (pos +! delta)
      where
        pos' = case delta of
            (0, dy) -> [pos +! (-1, dy), pos +! (0, dy), pos +! (1, dy)]
            (dx, 0) -> [pos +! (dx, -1), pos +! (dx, 0), pos +! (dx, 1)]
    allFreeAround (x, y) = all (flip S.notMember cs) around
      where
        around = [(x+dx, y+dy) | dx <- [(-1)..1], dy <- [(-1)..1], (dx, dy) /= (0, 0)]

moveProposed :: M.Map Pos [Pos] -> S.Set Pos
moveProposed proposals = M.foldlWithKey' moveOne S.empty proposals
  where
    moveOne ps dst srcs = case length srcs of
        1 -> S.insert dst ps
        _ -> S.union ps $ S.fromList srcs

bndBox :: S.Set Pos -> (Int, Int, Int, Int)
bndBox ps = (xmin, xmax, ymin, ymax)
  where
    xmin = S.findMin $ S.map fst ps
    xmax = S.findMax $ S.map fst ps
    ymin = S.findMin $ S.map snd ps
    ymax = S.findMax $ S.map snd ps

dumpField :: S.Set Pos -> String
dumpField ps = unlines $ map dumpLine [ymin..ymax]
  where
    (xmin, xmax, ymin, ymax) = bndBox ps
    dumpLine y = map (\x -> if S.member (x, y) ps then '#' else '.') [xmin..xmax]

steps :: S.Set Pos -> [Pos] -> [S.Set Pos]
steps i d = ith : steps ith (rotate 1 d)
  where
    ith = moveProposed $ proposeMove i d

day23'1 :: S.Set Pos -> Int
day23'1 fld = (xmax-xmin+1) * (ymax-ymin+1) - (S.size fld')
  where
    fld' = (steps fld [(0, -1), (0, 1), (-1, 0), (1, 0)]) !! 9
    (xmin, xmax, ymin, ymax) = bndBox fld'

day23'2 :: S.Set Pos -> Int
-- somewhat unelegant, but need to add 2 because tail and because
-- applying once already at steps !! 0
day23'2 i = (2+) . fromJust $ findIndex (uncurry (==)) $ zip s (tail s)
  where s = steps i [(0, -1), (0, 1), (-1, 0), (1, 0)]
