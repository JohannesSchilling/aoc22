import qualified Data.Set as S
import Data.List (minimumBy)
import Data.Function (on)

type Pos = (Int, Int)

main :: IO ()
main = do
    input <- lines <$> getContents
    let insts = parseInsts input
    print $ solve'1 insts S.empty (0, 0) (0, 0)
    print $ solve'2 insts S.empty $ take 10 $ repeat (0, 0)

parseInsts :: [String] -> String
parseInsts = concatMap (\(c:_:n) -> take (read n) $ repeat c)

solve'1 :: String -> S.Set Pos -> Pos -> Pos -> Int
solve'1 (i:is) visited h t = let
    h' = hstep i h
    t' = tstep h' t
  in
    solve'1 is (S.insert t' visited) h' t'
solve'1 [] visited _ _ = S.size visited

hstep :: Char -> Pos -> Pos
hstep inst (x, y) = case inst of
    'U' -> (x, y+1)
    'D' -> (x, y-1)
    'L' -> (x-1, y)
    'R' -> (x+1, y)
    _ -> error "unexpected step char"

dist (hx, hy) (tx, ty) = (abs $ hx - tx) + (abs $ hy - ty)
posAdd (ax, ay) (bx, by) = (ax+bx, ay+by)

tstep :: Pos -> Pos -> Pos
tstep h@(hx, hy) t@(tx, ty) = case dist h t of
        close | close <= 1 -> (tx, ty)
        -- grid dist 2 can be 2 in one straight direction, or diagonally
        -- touching.. case-distinction necessary
        2 | (abs $ hx - tx) == 1 -> (tx, ty)
        2 | hx == tx -> (tx, (hy+ty) `div` 2)
        2 | hy == ty -> ((hx+tx) `div` 2, ty)
        -- 3 means diagonal -> one of the four diagonal moves is good
        diag | diag `elem` [3,4] -> minimumBy (compare `on` dist h) $ map (posAdd t) [(1, 1), (-1, 1), (1, -1), (-1, -1)]
        _more -> error "too far away, previous calculation error"

solve'2 :: String -> S.Set Pos -> [Pos] -> Int
solve'2 (i:is) visited (p:ps) = let
    h' = hstep i p
    (ts, v) = foldl step ([], h') ps
    step (acc, h') t = let t' = tstep h' t in ((t':acc), t')
  in
    solve'2 is (S.insert v visited) (h':(reverse ts))
solve'2 [] visited _ = S.size visited
