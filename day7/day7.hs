{-# LANGUAGE ViewPatterns #-}

import qualified Data.Map as M
import Data.List (stripPrefix, sortBy, sort)
import Data.Maybe (fromJust)
import Data.Function (on)

main :: IO ()
main = do
    input <- lines <$> readFile "sample.txt"
    let (ds, fs) = collect input [] M.empty M.empty
    -- the "sort by length of parent dir list reverse" is a poor man's
    -- topological sort; i.e. if we're sure we fill in the lower levels
    -- of the directory tree first, we are sure we will have them
    -- covered when filling in the parent dirs
    let allDirs = retrofill (reverse $ sortBy (compare `on` (length . fst)) $ M.toList ds) fs
    putStrLn . ("easy: " ++) . show $ solveEasy allDirs
    putStrLn . ("hard: " ++) . show $ solveHard allDirs

collect :: [String] -> [String] -> M.Map [String] [String] -> M.Map [String] Int -> (M.Map [String] [String], M.Map [String] Int)
collect ((stripPrefix "$ cd " -> Just subdir):cs) pwd dirs sizes = case subdir of
    ".." -> collect cs (tail pwd) dirs sizes
    other -> collect cs (other:pwd) dirs $ M.alter (maybe (Just 0) Just) (other:pwd) sizes
-- ignore the ls command itself, we just parse the output
collect ((stripPrefix "$ ls" -> Just _):cs) pwd dirs sizes = collect cs pwd dirs sizes
collect ((stripPrefix "dir " -> Just dirName):cs) pwd dirs sizes = let
    dirs' = M.alter (maybe (Just [dirName]) (Just . (dirName:))) pwd dirs
    sizes' = M.alter (maybe (Just 0) Just) (dirName:pwd) sizes
  in
    collect cs pwd dirs' sizes'
-- other entry can only be "[size] [filename]"
collect (c:cs) pwd dirs sizes = let size = read . head $ words c in collect cs pwd dirs (M.insertWith (+) pwd size sizes)
-- stop when out of commands
collect [] pwd dirs sizes = (dirs, sizes)

-- add subdir sizes in total size calculation
retrofill :: [([String], [String])] -> M.Map [String] Int -> M.Map [String] Int
retrofill ((parent, elems):ds) sizes = let
    fullPaths = map (:parent) elems
    getSize p = case M.lookup p sizes of
        Just s -> s
        Nothing -> error $ "no info for " ++ show p ++ ", only have " ++ show sizes
    elemSize = sum $ map getSize fullPaths
  in
    retrofill ds $ M.adjust (+ elemSize) parent sizes
retrofill [] sizes = sizes

solveEasy :: M.Map [String] Int -> Int
solveEasy = M.foldr (+) 0 . M.filter (<= 100000)

solveHard :: M.Map [String] Int -> Int
solveHard dirData = let
    totalUsed = fromJust $ M.lookup ["/"] dirData
    totalUnused = 70000000 - totalUsed
    requiredFree = 30000000 - totalUnused
  in
    head . dropWhile (< requiredFree) . sort $ M.elems dirData
