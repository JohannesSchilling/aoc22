{-# LANGUAGE LambdaCase, BlockArguments #-}

main :: IO ()
main = do
    input <- words <$> readFile "input.txt"
    putStrLn $ "easy: " ++ (show $ solveEasy input)
    putStrLn $ "hard: " ++ (show $ solveHard input)

data Shape = Rock | Paper | Scissors
  deriving (Show, Eq)

data Outcome = Loose | Draw | Win
  deriving (Show, Eq)

solveEasy :: [String] -> Integer
solveEasy = go 0 . map shape
  where
    go acc (x:y:xs) = go (acc + game x y) xs
    go acc []       = acc
    go acc [x]      = error "uneven list"

solveHard :: [String] -> Integer
solveHard = go 0
  where
    go acc (x:y:xs) = go (acc + predictedGame (shape x) (outcome y)) xs
    go acc []       = acc
    go acc [x]      = error "uneven list"

    predictedGame :: Shape -> Outcome -> Integer
    predictedGame theirs goal = let
      mine = case goal of
        Win -> win theirs
        Draw -> draw theirs
        Loose -> loose theirs
      in
        game theirs mine

    win = \case
        Rock -> Paper
        Paper -> Scissors
        Scissors -> Rock

    draw = id

    loose = \case
        Rock -> Scissors
        Paper -> Rock
        Scissors -> Paper

game theirs mine = shapescore mine + gamescore theirs mine

shape x | x `elem` ["A", "X"] = Rock
        | x `elem` ["B", "Y"] = Paper
        | x `elem` ["C", "Z"] = Scissors
shape _ = error "invalid shape"

outcome "X" = Loose
outcome "Y" = Draw
outcome "Z" = Win

shapescore = \case
    Rock     -> 1
    Paper    -> 2
    Scissors -> 3

gamescore = curry \case
    (Rock, Scissors)     -> 0
    (Paper, Rock)        -> 0
    (Scissors, Paper)    -> 0
    (Rock, Rock)         -> 3
    (Paper, Paper)       -> 3
    (Scissors, Scissors) -> 3
    (Scissors, Rock)     -> 6
    (Rock, Paper)        -> 6
    (Paper, Scissors)    -> 6

