import qualified Data.Vector as V
import Data.List.Split (splitOn, split, keepDelimsL, whenElt)
import Data.Char (isAlpha)
import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace (trace)

main :: IO ()
main = do
    (m, insts) <- readInput <$> getContents
    putStrLn $ (++) "day22'1: " $ show $ day22'1 m insts
    --putStrLn $ (++) "day22'2: " $ show $ day22'2 input

type Map = V.Vector (V.Vector Char)
data Inst = Move Int
          | Rotate Char
    deriving (Show, Eq)

readInput :: String -> (Map, [Inst])
readInput inp = let
    (m:insts:_) = splitOn "\n\n" inp
    vm = V.fromList $ map V.fromList $ lines m
    (i:is) = split (keepDelimsL $ whenElt isAlpha) insts
    is' = (Move $ read i) : concatMap (\(r:n) -> [Rotate r, Move $ read n]) is
  in
    (vm, is')

type Pos = (Int, Int)

findStart :: Map -> Pos
findStart m = (fromJust $ V.elemIndex '.' $ m V.! 0, 0)

data Dir = U | R | D | L
    deriving (Show, Eq, Enum)

at m (x, y) = fromMaybe ' ' $ (m V.! y) V.!? x
--at m (x, y) = repair $ (m V.! y) V.!? x
--  where
--    repair (Just x) = x
--    repair Nothing = trace ("oob at " ++ show (x, y)) ' '

void m x y = m `at` (x, y) == ' '

--skipVoid _ x y _ _ | trace (show (x, y)) False = undefined
skipVoid m x y dx dy = let
    xlen = V.length $ m V.! 0
    xm = x `mod` xlen
    x' = (x + dx + xlen) `mod` xlen
    ylen = V.length m
    ym = y `mod` ylen
    y' = (y + dy + ylen) `mod` ylen
  in
    case m `at` (xm, ym) of
        ' ' -> skipVoid m x' y' dx dy
        _ -> (xm, ym)

nextPos :: Map -> Pos -> Dir -> Pos
nextPos m (x, y) dir = let
    (dx, dy) = case dir of
        U -> (0, -1)
        R -> (1, 0)
        D -> (0, 1)
        L -> (-1, 0)
  in
    skipVoid m (x+dx) (y+dy) dx dy

rot :: Dir -> Char -> Dir
rot L 'R' = U
rot d 'R' = succ d
rot U 'L' = L
rot d 'L' = pred d
rot od oc = error $ (oc:", ") ++ show od

walk :: Map -> [Inst] -> Dir -> (Pos, Dir)
walk map insts d = go map (findStart map) d insts
  where
    go :: Map -> Pos -> Dir -> [Inst] -> (Pos, Dir)
    --go _ pos d (i:_) | trace (show pos ++ ", " ++ show d ++ ", " ++ show i) False = undefined
    go m pos d [] = (pos, d)
    go m pos d ((Rotate dd):is) = go m pos (rot d dd) is
    go m pos d ((Move 0):is) = go m pos d is
    go m pos d ((Move n):is) = let np = nextPos m pos d in case m `at` np of
            '#' -> go m pos d is
            '.' -> go m np d ((Move $ n - 1):is)

day22'1 :: Map -> [Inst] -> Int
day22'1 m is = let
    ((x, y), d) = walk m is R
  in
    1000 * (y+1) + 4 * (x+1) + (fromEnum d)-1
