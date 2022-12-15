import Data.List.Split (splitOn, chunksOf)
import Data.List (foldl', maximumBy)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Function (on)
import Debug.Trace (trace)
import GHC.Exts (the)

import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Char


main :: IO ()
main = do
    input <- lines <$> getContents
    let sensors = map parseSensor input
    --putStrLn . ("day15'1: " ++) . show $ day15'1 2000000 sensors
    putStrLn . ("day15'2: " ++) . show $ day15'2 4000000 sensors


data Pos = Pos {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Show, Eq)

dist (Pos a b) (Pos c d) = abs (a-c) + abs (b-d)

data Sensor = Sensor {-# UNPACK #-} !Pos {-# UNPACK #-} !Pos
  deriving (Show, Eq)

parseSensor :: String -> Sensor
parseSensor l = case parse parser "" l of
    Left err -> error $ show err
    Right s -> s
  where
    parser = Sensor <$> (string "Sensor at " *> xy) <*> (string ": closest beacon is at " *> xy)
    xy = Pos <$> (string "x=" *> num) <*> (string ", y=" *> num)
    num = read <$> many1 (digit <|> char '-')

noSensorForY :: Int -> Sensor -> [Int]
noSensorForY y (Sensor s@(Pos sx sy) b@(Pos bx by)) = let
    free = dist s b - 1
    ydist = abs $ sy - y
    overlap = free - ydist + 1
  in
    if overlap < 0 then [] else [(sx-overlap)..(sx+overlap)]

noSensorsForY :: Int -> [Sensor] -> S.Set Int
noSensorsForY y s = foldl' S.union S.empty $ map (S.fromList . noSensorForY y) s

day15'1 :: Int -> [Sensor] -> Int
day15'1 y sensors = let
    space = S.size $ noSensorsForY y sensors
    xpos (Pos x _) = x
    ypos (Pos _ y) = y
    bcn (Sensor _ b) = b
    beacons = S.size . S.fromList $ map xpos . filter ((== y) . ypos) . map bcn $ sensors
  in
    space - beacons

inRange :: Sensor -> Pos -> Bool
inRange (Sensor s b) pos = dist s pos <= dist s b


-- this is too slow on big things..
around :: Sensor -> Int -> [Pos]
around (Sensor s@(Pos sx sy) b) cmax = let
    d = dist s b
    xrange = [x | x <- [(sx-d-1)..(sx+d+1)], 0 <= x, x < cmax]
    yrange = [y | y <- [(sy-d-1)..(sy+d+1)], 0 <= y, y < cmax]
  in
    [Pos x y | x <- xrange, y <- yrange, dist s (Pos x y) == (d+1)]

-- .. let's hand-write what we need instead
around' :: Sensor -> Int -> [Pos]
around' (Sensor s@(Pos sx sy) b) cmax = let
    d = dist s b + 1
    line (a, b) (c, d) = zip [a..c] [b..d]
    -- need to take care to always have a positive range
    nw = zip [(sx-d)..sx] (reverse [(sy-d)..sy])
    ne = zip [sx..(sx+d)] [(sy-d)..sy]
    se = zip (reverse [sx..(sx+d)]) [sy..(sy+d)]
    sw = zip [(sx-d)..sx] [sy..(sy+d)]
    inBounds (a, b) = a >= 0 && a <= cmax && b >= 0 && b <= cmax
  in
    map (uncurry Pos) . filter inBounds $ concat [nw, ne, se, sw]

day15'2 :: Int -> [Sensor] -> Int
day15'2 cmax sensors = let
    fringes = concatMap (flip around' cmax) sensors
    unreachable = filter (\pos -> not $ any (flip inRange pos) sensors) $ fringes
    freq (Pos x y) = 4000000 * x + y
  in
    freq $ the unreachable
