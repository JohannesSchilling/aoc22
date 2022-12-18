{-# LANGUAGE ViewPatterns #-}

import qualified Data.Set as S
import Control.Monad (guard)
import Debug.Trace (trace)
import Data.List (foldl')
import Data.Tuple.Extra

main :: IO ()
main = do
    input <- map p3 . lines <$> getContents
    print $ day18'1 input
    print $ day18'2 input

type Pt = (Int, Int, Int)

p3 :: String -> Pt
p3 = read . ('(':) . (++ ")")

(a, b, c) +! (d, e, f) = (a+d, b+e, c+f)

nbrs :: Pt -> [Pt]
nbrs pt = [pt +! (-1, 0, 0), pt +! (1, 0, 0),
           pt +! (0, -1, 0), pt +! (0, 1, 0),
           pt +! (0, 0, -1), pt +! (0, 0, 1)]

freeNbrs :: S.Set Pt -> (Pt -> Bool) -> Pt -> Int
freeNbrs pts extraCond pt = sum $ do
    pt' <- nbrs pt
    -- guard $ trace (show pt ++ " '-> " ++ show pt' ++ ": " ++ (show $ (pt' `S.notMember` pts, extraCond pt'))) True
    guard $ pt' `S.notMember` pts
    guard $ extraCond pt'
    return 1

day18'1 :: [Pt] -> Int
day18'1 pts = let
    ptsSet = S.fromList pts
  in
    sum $ map (freeNbrs ptsSet (const True)) pts

boundingBox :: [Pt] -> (Int, Int, Int, Int, Int, Int)
boundingBox pts = let
    xmin = minimum $ map fst3 pts
    xmax = maximum $ map fst3 pts
    ymin = minimum $ map snd3 pts
    ymax = maximum $ map snd3 pts
    zmin = minimum $ map thd3 pts
    zmax = maximum $ map thd3 pts
  in
    -- add 1 around the edge, so all outside will be one flood component
    (xmin-1, xmax+1, ymin-1, ymax+1, zmin-1, zmax+1)

inBndBox :: (Int, Int, Int, Int, Int, Int) -> Pt -> Bool
inBndBox (xmin, xmax, ymin, ymax, zmin, zmax) (x, y, z) = xmin <= x && x <= xmax && ymin <= y && y <= ymax && zmin <= z && z <= zmax

floodOutside pts = let
    bbx@(xmin, _, ymin, _, zmin, _) = boundingBox pts
    outside = S.empty
    todo = S.singleton (xmin, ymin, zmin)
    ptsSet = S.fromList pts
    go :: S.Set Pt -> S.Set Pt -> S.Set Pt
    -- go reachable _ | trace (show reachable) False = undefined
    go reachable s | S.null s = reachable
    go reachable (S.minView -> Just (t, todo)) = let
        n = filter (flip S.notMember ptsSet) $ filter (flip S.notMember reachable) $ nbrs t
        nb = filter (inBndBox bbx) n
      in
        go (S.insert t reachable) (S.union todo $ S.fromList nb)
  in
    go outside todo

day18'2 :: [Pt] -> Int
day18'2 pts = let
    ptsSet = S.fromList pts
    outside = floodOutside pts
    extraCond = flip S.member outside
  in
    sum $ map (freeNbrs ptsSet extraCond) pts
