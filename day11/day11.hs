import qualified Data.Map.Strict as M
import Data.List.Split
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.List (stripPrefix)
import Data.Bool.HT (if')
import Data.List (scanl1')

main :: IO ()
main = do
    input <- map parseMonkey . splitOn "\n\n" <$> getContents
    print $ length input

parseMonkey :: String -> ([Int], (Int -> Int), (Int -> Int))
parseMonkey s = let
    ls = tail . map ((!! 2) . splitOn ":") $ lines s
    starts = map read . splitOn ", " $ ls !! 0
    op = case stripPrefix " new = old " $ ls !! 1 of
        Just ('+':n) -> (+ read n)
        Just ('*':n) -> (* read n)
        Nothing -> error "you're dumb"
        otherwise -> error "unknown op"
    next x = let
        (d:t:f:_) = map (read . ((concatMap words $ drop 2 ls) !!)) [2,6,10]
      in
        if' (x `mod` d == 0) t f
  in
    (starts, op, next)

mr [] _ _ count nms = (count, nms)
mr (i:<|is) op next count nms = let
    nextIx = next $ op i `div` 3
  in
    mr is op next (count+1) (M.update (S.|> i) nextIx)

round ix (m:ms) (o:os) (n:ns) (c:cs) = let
    (c, newms) = mr m o n c

day11'1 :: [([Int], (Int -> Int), (Int -> Int))] -> Int
day11'1 input = let
    (items, ops, nexts) = unzip input

