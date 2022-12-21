import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Char
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)

main :: IO ()
main = do
    input <- map parseEqn . lines <$> getContents
    putStrLn $ (++) "day21'1: " $ show $ day21'1 input
    putStrLn $ (++) "day21'2: " $ show $ day21'2 input

data Expr = I Int | B String Char String
    deriving (Show, Eq)

parseEqn :: String -> (String, Expr)
parseEqn e = either (error . show) id $ parse parser "" e
  where
    parser = (,) <$> name <* char ':' <* spaces <*> (try num <|> binop)
    num = I . read <$> many1 (digit <|> char '-')
    binop = B <$> name <*> (oneOf "+-*/" <* spaces) <*> name
    name = many1 letter <* spaces

toHsOp = fromJust . flip lookup ops
  where
    ops = [('+', (+)), ('-', (-)), ('*', (*)), ('/', div)]

eval :: M.Map String Expr -> Expr -> Int
eval _ (I i) = i
eval m (B l op r) = let
    l' = eval m (m M.! l)
    r' = eval m (m M.! r)
  in
    (toHsOp op) l' r'

day21'1 :: [(String, Expr)] -> Int
day21'1 input = eval m (m M.! "root")
  where
    m = M.fromList input

data Subtree = L | R | N
    deriving (Show, Eq)

findHuman :: M.Map String Expr -> String -> [Subtree]
findHuman m e = case m M.! e of
    (I _) -> [N]
    (B l _ _) | l == "humn" -> [L]
    (B _ _ r) | r == "humn" -> [R]
    (B l _ r) -> let
        l' = findHuman m l
        r' = findHuman m r
        found x = (head x) `elem` [L, R]
      in
        if found l' then L:l' else if found r' then R:r' else [N]

data Evaluated = Calc Int
               | Human Int
    deriving (Show, Eq)

evalOrFillIn :: M.Map String Expr -> [Subtree] -> String -> Int -> Evaluated
evalOrFillIn _ [] "humn" target = Human target
evalOrFillIn m (L:st) expr target = let
    (B l op r) = m M.! expr
    r' = eval m (m M.! r)
    target' = case op of
        '+' -> target - r'
        '-' -> target + r'
        '*' -> target `div` r'
        '/' -> target * r'
  in
    evalOrFillIn m st l target'
evalOrFillIn m (R:st) expr target = let
    (B l op r) = m M.! expr
    l' = eval m (m M.! l)
    target' = case op of
        '+' -> target - l'
        -- l' - t' == t -> t' == l' - t
        '-' ->  l' - target
        -- l' * t' == t -> t' = t / l'
        '*' -> target `div` l'
        -- l' / t' == t -> t' == l' / t
        '/'  -> l' `div` target
  in
    evalOrFillIn m st r target'


day21'2 :: [(String, Expr)] -> Int
day21'2 input = let
    m = M.fromList input
    (B l _ r) = m M.! "root"
    (p:ps) = findHuman m "root"
    r' = eval m $ m M.! r
    l' = eval m $ m M.! l
  in
    case p of
        L -> let (Human n) = evalOrFillIn m ps l r' in n
        R -> let (Human n) = evalOrFillIn m ps r l' in n 
        N -> error "human not found, it's all robots"
