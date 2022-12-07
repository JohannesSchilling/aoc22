import Data.Set (fromList, size)
import Data.List (tails)

main :: IO ()
main = lines <$> readFile "input.txt" >>= print . map (\l -> (take 4 l, map (flip solve l) [4,14]))

solve :: Int -> [Char] -> Int
solve winsize = fst . head . dropWhile ((/= winsize) . size . fromList . snd) . zip [winsize..] . windows
  where
    -- this simple definition has trailing shorter sequences, but we
    -- don't care for this use-case
    windows = map (take winsize) . tails
