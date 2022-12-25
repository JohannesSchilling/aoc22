{-# LANGUAGE LambdaCase #-}

main :: IO ()
main = do
    input <- lines <$> getContents
    putStrLn $ (++) "day25'1: " $ day25'1 input

snafuToDec :: String -> Int
snafuToDec = foldl (\acc d -> (5*acc) + toD d) 0

toD = \case
    '2' -> 2
    '1' -> 1
    '0' -> 0
    '-' -> -1
    '=' -> -2

fromD = \case
    12 -> ('2', '2')
    11 -> ('2', '1')
    10 -> ('2', '0')
    9 -> ('2', '-')
    8 -> ('2', '=')
    7 -> ('1', '2')
    6 -> ('1', '1')
    5 -> ('1', '0')
    4 -> ('1', '-')
    3 -> ('1', '=')
    2 -> ('0', '2')
    1 -> ('0', '1')
    0 -> ('0', '0')
    neg | neg < 0 -> let (c, d) = fromD (-neg) in (snafuNegate c, snafuNegate d)
    oob | oob > 5 -> error $ "out of bounds for single convert: " ++ show oob

snafuNegate '2' = '='
snafuNegate '1' = '-'
snafuNegate '0' = '0'
snafuNegate '-' = '1'
snafuNegate '=' = '2'

addCarry :: String -> Char -> String
addCarry n c = go (reverse n) c ""
  where
    go [] '0' acc = acc
    go [] '1' acc = '1' : acc
    go [] '2' acc = '2' : acc
    go n '0' acc = reverse n ++ acc
    go ('2':ns) '2' acc = go ns '1' ('-':acc)
    go ('2':ns) '1' acc = go ns '1' ('=':acc)
    go ('=':ns) '-' acc = go ns '-' ('2':acc)
    go ('=':ns) '=' acc = go ns '-' ('1':acc)
    -- can ignore the carry from `fromD` here because we already treated all
    -- the overflowing cases, so it will be '0' always
    go (n:ns) c acc = reverse ns ++ ((snd . fromD $ (toD n) + (toD c)) : acc)

decToSnafu :: Int -> String
decToSnafu 0 = ""
decToSnafu n | -12 <= n && n <= 12 = let
    (c, d) = fromD n
  in
    if c == '0' then [d] else [c, d]
decToSnafu n = let
    h = decToSnafu (n `div` 5)
    (c, t) = fromD (n `mod` 5)
  in
    addCarry h c ++ [t]

day25'1 :: [String] -> String
day25'1 = decToSnafu . sum . map snafuToDec 
