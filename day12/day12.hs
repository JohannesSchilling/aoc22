{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import qualified Data.Heap as H
import qualified Data.Set as S

import Data.Word
import Control.Monad
import Data.List (foldl')
import Data.Maybe (fromJust, isJust)

main :: IO ()
main = do
    input <- readGrid <$> B.getContents
    putStrLn $ (++) "day12'1: " $ show $ day12'1 input
    putStrLn $ (++) "day12'2: " $ show $ day12'2 input

data Pos = Pos !Int !Int
    deriving (Show, Eq, Ord)

data Grid = Grid [B.ByteString]
    deriving (Show, Eq, Ord)

readGrid :: B.ByteString -> Grid
readGrid = Grid . B.split '\n' . B.dropWhileEnd (== '\n') 

at :: Grid -> Pos -> Char
at (Grid ls) (Pos x y) = (ls !! y) `B.index` x

dims :: Grid -> (Int, Int)
dims (Grid ls) = (B.length $ ls !! 0, length ls)

canWalk from to grid = (grid `elev` to) - (grid `elev` from)  <= 1

elev grid pos = fromEnum $ case grid `at` pos of
    'S' -> 'a'
    'E' -> 'z'
    x -> x

buildGraph :: Grid -> M.Map Pos [Pos]
buildGraph grid = let
    (width, height) = dims grid
    poss = [Pos x y | x <- [0..(width-1)], y <- [0..(height-1)]]
    nbrs :: Pos -> [Pos]
    nbrs p@(Pos x y) = do
        p'@(Pos x' y') <- [Pos (x-1) y, Pos (x+1) y, Pos x (y-1), Pos x (y+1)]
        guard $ 0 <= x' && x' < width
        guard $ 0 <= y' && y' < height
        guard $ canWalk p p' grid
        return p'
  in
    foldl' (\m p -> M.insert p (nbrs p) m) M.empty poss

type Q = H.MinPrioHeap Int Pos

bfs :: M.Map Pos [Pos] -> S.Set Pos -> Q -> Pos -> M.Map Pos Int -> M.Map Pos Int
bfs _adj _visited q _goal dists | H.null q = error "goal not found"
bfs adj visited (H.view -> Just ((_, me), qs)) goal dists | me == goal = dists
-- ignore if already visited -- huge speedup
bfs adj visited (H.view -> Just ((_, me), qs)) goal dists | me `S.member` visited = bfs adj visited qs goal dists
bfs adj visited (H.view -> Just ((dMe, me), qs)) goal dists = let
    v' = me `S.insert` visited
    (d', q') = foldl' updateDists (dists, qs) $ adj M.! me
    updateDists :: (M.Map Pos Int, Q) -> Pos -> (M.Map Pos Int, Q)
    updateDists (dists, q) pos = let
        d' = M.insertWith min pos (dMe+1) dists
        q' = if pos `S.notMember` v' then H.insert (d' M.! pos, pos) q else q
      in
        (d', q')
  in
    bfs adj v' q' goal d'

findGrid :: Grid -> Char -> [Pos]
findGrid (Grid ls) c = do
    y <- [0..(length ls - 1)]
    x <- B.elemIndices c (ls !! y)
    return $ Pos x y

day q d g end = let
    adj = buildGraph g
    dists = bfs adj S.empty q end M.empty
  in
    dists M.! end
    
day12'1 :: Grid -> Int
day12'1 g = let
    (start, end) = (head $ findGrid g 'S', head $ findGrid g 'E')
    q = H.singleton (0, start)
  in
    day q M.empty g end

day12'2 :: Grid -> Int
day12'2 g = let
    (start, end) = (head $ findGrid g 'S', head $ findGrid g 'E')
    starts = start : findGrid g 'a'
    q = H.fromList $ map (0,) starts
    d = M.fromList $ map (,0) starts
  in
    day q d g end
