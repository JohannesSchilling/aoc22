import Data.List
import System.IO

main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    putStrLn $ "easy: " ++ (show $ solveEasy input)
    putStrLn $ "hard: " ++ (show $ solveHard input)

solveEasy = maximum . sumGroups

solveHard = sum . take 3 . reverse . sort . sumGroups

sumGroups xs = go 0 xs
go acc (x:xs) = if x == "" then acc : go 0 xs else go (acc + read x) xs
go acc [] = [acc]
