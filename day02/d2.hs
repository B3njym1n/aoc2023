import Data.List
import Data.Semigroup
import Text.Parsec

str2int :: [Char] -> Int
str2int = read

data Set = Set {
  red :: Int,
  green :: Int,
  blue :: Int
  } deriving (Show, Eq)

instance Semigroup Set where
  Set r1 g1 b1 <> Set r2 g2 b2 = Set (r1 + r2) (g1 + g2) (b1 + b2)
  

data Game = Game {
  gid :: Int,
  sets :: [Set]
            } deriving (Show, Eq)

parseInt :: Parsec String () Int
parseInt = do
  x <- many digit
  return (str2int x)

parseGameId = do
  id <- string "Game" >> many space >> parseInt
  char ':'
  return id

toCube :: Int -> String -> Set
toCube n color = case color of
  "red" -> Set n 0 0
  "green" -> Set 0 n 0
  "blue" -> Set 0 0 n

parseCube = do
   many space
   cnt <- parseInt
   many space
   color <- string "red" <|> string  "blue" <|> string "green"
   return (toCube cnt color)


parseSet = do
  lst <- parseCube `sepBy` char ','
  return (foldl (<>) (Set 0 0 0) lst)

parseSet3 = do
  lst <- parseSet `sepBy` char ';'
  return lst

parseLine = do
  id <- parseGameId
  ss <- parseSet3
  return (Game id ss)

parseLines = do
  games <- parseLine `sepEndBy` newline
  return games

part01 :: [Game] -> Int
part01 games = sum (map f filterd)
  where filterd = filter judge games
        f game = gid game

judge :: Game -> Bool
judge game = all f ss
  where ss = sets game
        f (Set a b c) | a <= 12 && b <= 13 && c <= 14 = True
                      | otherwise = False

minif :: [Set] -> (Max Int, Max Int, Max Int)
minif xs = foldl f (Max 0, Max 0, Max 0) xs
  where f (a, bb, c) (Set r g b) = (a <> Max r, bb <> Max g, c <> Max b)

power :: (Max Int, Max Int, Max Int) -> Int
power (r, g, b) = getMax r * getMax g * getMax b

part02 :: [Game] -> Int
part02 = sum . map f
  where f = power . minif . sets

solve :: [Game] -> [Int]
solve game = [part01 game, part02 game]

display :: [Int] -> IO ()
display = mapM_ print

main = do
  lines <- readFile "input"
  either print (display . solve) (parse parseLines "Games" lines)

