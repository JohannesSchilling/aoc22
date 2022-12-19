import Data.Word
import Data.Bits (testBit, shiftL, shiftR, (.&.), (.|.))
import Debug.Trace (trace)
import qualified Data.Map as M

main :: IO ()
main = do
    i <- filter (`elem` "<>") <$> getContents
    putStrLn $ (++) "day17'1: " $ show $ day17'1 i
    putStrLn $ (++) "day17'2: " $ show $ day17'2 i

type Piece = [Word8]
type Field = [Word8]

-- the given pieces, encoded from bottom to top, since bottom hits field first
pieces :: [Piece]
pieces = [
    -- ..####.
    [
        30
    ],
    -- ...#...
    -- ..###..
    -- ...#...
    [
        8,
        28,
        8
    ],
    -- ....#..
    -- ....#..
    -- ..###..
    [
        28,
        4,
        4
    ],
    -- ..#....
    -- ..#....
    -- ..#....
    -- ..#....
    [
        16,
        16,
        16,
        16
    ],
    -- ..##...
    -- ..##...
    [
        24,
        24
    ]]

showBits n = foldr (\i acc -> if testBit n i then '#':acc else '.':acc) "" [6,5..0]

dbgPiece :: Piece -> IO ()
dbgPiece p = putStrLn . unlines . reverse $ map showBits p

dbgField :: Field -> IO ()
dbgField = putStrLn . unlines . map showBits


fieldFreeAt :: Field -> Piece -> Bool
fieldFreeAt f p = all (== 0) $ zipWith (.&.) f p

pieceCanLeft :: Piece -> Bool
pieceCanLeft p = all (== False) $ map (flip testBit 6) p

pieceCanRight :: Piece -> Bool
pieceCanRight p = all (== False) $ map (flip testBit 0) p

push :: Char -> Field -> Piece -> Piece
push '<' srnd p | pieceCanLeft p && fieldFreeAt srnd p' = p'
  where
    p' = map (flip shiftL 1) p
push '<' _ p = p
push '>' srnd p | pieceCanRight p && fieldFreeAt srnd p' = p'
  where
    p' = map (flip shiftR 1) p
push '>' _ p = p

dropPiece :: Piece -> [Char] -> Field -> ([Char], Field)
dropPiece p (g0:g1:g2:gs) f = let
    p3 = push g2 [] $ push g1 [] $ push g0 [] p
  in
    doPush p3 gs f [] 

mergeBackward :: Piece -> Field -> Field
mergeBackward p bw = let
    merged = zipWith (.|.) bw p
    l = length merged
  in
    -- add remainder of whichever one was longer to the end
    -- (i.e. one of those two is always empty)
    reverse $ merged ++ drop l bw ++ drop l p

landed p (f:_) bw = any (/= 0) $ zipWith (.&.) p (f:bw)

doPush p (g:gs) fw bw = doDrop (push g bw p) gs fw bw

doDrop p gs [] bw = (gs, mergeBackward p bw)
doDrop p gs fw@(f:_) bw | landed p fw bw = (gs, mergeBackward p bw ++ fw)
doDrop p gs (f:fw) bw = doPush p gs fw (f:bw)

day :: [Char] -> [([Char], Field)]
day g = let
    gs = cycle g
    ps = cycle pieces
  in
    scanl (\(g, f) p -> dropPiece p g f) (gs, []) ps

day17'1 :: [Char] -> Int
day17'1 g = length . snd $ (day g) !! 2022

-- haha ;-)
-- day17'2 :: [Char] -> Int
-- day17'2 g = day g 1000000000000


day17'2 g = let
    fs = day g
    (lo, hi) = detectCycle fs
    hgt n = length . snd $ fs !! n
    cycleLen = hi - lo
    cycleStart = lo -- or maybe before, but still calculatable territory
    heightStart = hgt cycleStart 
    heightCycle = hgt hi - heightStart
    remBlocks = (1000000000000 - cycleStart) `mod` cycleLen
    numCycles = (1000000000000 - cycleStart) `div` cycleLen
    remHeight = hgt (cycleStart + remBlocks) - heightStart
  in
    heightStart + numCycles * heightCycle + remHeight

detectCycle :: [(String, Field)] -> (Int, Int)
detectCycle fs = go (zip [0..] fs) M.empty
  where
    go :: [(Int, (String, Field))] -> M.Map Integer (Int, Field) -> (Int, Int)
    go ((i, (g, f)):fs) m = let
        s1000 = sum $ map toInteger $ take 1000 f
        continue = go fs (M.insert s1000 (i, f) m)
      in
        case M.lookup s1000 m of
            Just (i', prev) -> let
                lengthEq = length $ takeWhile id $ zipWith (==) prev f
              in
                if lengthEq > 5000 then (i', i) else continue
            Nothing -> continue 
    go [] m = error "sad"
