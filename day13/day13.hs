import Data.List.Split hiding (sepBy)
import Data.Either.Utils (fromRight)
import Data.Function (on)
import Data.Maybe (fromJust)
import Data.List (elemIndex, sort)

import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Char

main :: IO ()
main = do
    input <- splitOn "\n\n" <$> getContents
    putStrLn $ "day1'1: " ++ (show $ day13'1 input)
    putStrLn $ "day1'2: " ++ (show $ day13'2 input)

data List = I Int
          | L [List]
          deriving (Show, Eq)

parseListLine :: String -> List
parseListLine l = case parse parser "" l of
    Left err -> error $ show err
    Right l -> l
  where
    parser = list <|> int
    int = I . read <$> many1 digit
    list = between (char '[') (char ']') $ L <$> sepBy parser (char ',')

instance Ord List where
    (I a) `compare` (I b) = a `compare` b
    (L as) `compare` (L bs) = as `compare` bs
    (L as) `compare` (I b) = as `compare` [I b]
    (I a) `compare` (L bs) = [I a] `compare` bs

chunkOrdered :: String -> Bool
chunkOrdered s = let
    (l:r:_) = map parseListLine $ lines s
  in
    l < r

day13'1 :: [String] -> Int
day13'1 = sum . map fst . filter (chunkOrdered . snd) . zip [1..]

day13'2 :: [String] -> Int
day13'2 chunks = let
    d1 = L [L [I 2]]
    d2 = L [L [I 6]]
    signals = sort $ d1:d2:concatMap (map parseListLine . lines) chunks

  in
    ((+) 1 $ fromJust $ elemIndex d1 signals) * ((+) 1 $ fromJust $ elemIndex d2 signals)

