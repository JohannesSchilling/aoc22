import qualified Data.Sequence as SQ
import Data.Sequence ((><), Seq(..))
import Data.Maybe (fromJust)
import Data.Int
import Debug.Trace (trace)

main :: IO ()
main = do
    input <- map read . lines <$> getContents
    print $ day20'1 $ SQ.fromList input 
    print $ day20'2 input 

indexed s = SQ.mapWithIndex (,) s

mix :: Seq (Int, Int64) -> Seq (Int, Int64)
mix s = go s 0
  where
    l = SQ.length s
    newPos :: Int -> Int64 -> Int
    newPos o v = fromIntegral $ ((fromIntegral o) + v - 1) `mod` ((fromIntegral l)-1) + 1
    go :: Seq (Int, Int64) -> Int -> Seq (Int, Int64)
    go s i | i == l = s 
    go s i = let
        ix = fromJust $ SQ.findIndexL ((== i) . fst) s
        v = snd $ s `SQ.index` ix
        ix' = newPos ix v
      in
        go (SQ.insertAt ix' (i, v) $ SQ.deleteAt ix s) (i+1)

day20'common :: Seq (Int, Int64) -> Int64
day20'common mixed = let
    mixed' = fmap snd mixed
    zpos = fromJust $ SQ.findIndexL (== 0) mixed'
    len = SQ.length mixed'
  in
    sum $ map (mixed' `SQ.index`) [(zpos+1000) `mod` len, (zpos+2000) `mod` len, (zpos+3000) `mod` len]

day20'1 :: Seq Int64 -> Int64
day20'1 = day20'common . mix . indexed

day20'2 :: [Int64] -> Int64
day20'2 s = let
    ms = map (* 811589153) s
    mixed = foldl (\s _ -> mix s) (indexed $ SQ.fromList ms) [0..9]
  in
    day20'common mixed
