import Data.Char
import Data.Array
import qualified Data.List as L
import qualified Data.Set as S
import Control.Monad

example = ["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53", "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19", "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1", "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83", "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36", "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"]

toInt :: String -> Int
toInt = read

{--
readInputToLines :: FilePath -> IO [String]
readInputToLines fileName = lines <$> (readFile fileName)
--}

splitByChar :: Char -> String -> (String, String)
splitByChar c s = (part1, part2)
  where indices = L.findIndices (==c) s
        index   = head indices
        (part1, rest) = L.splitAt index s
        part2 = tail rest


splitLine :: String -> ([Int],[Int])
splitLine line = (wining_nums, your_nums)
  where (cardhead, nums) = splitByChar ':' line
        (winpart, yourpart) = splitByChar '|' nums
        wining_nums = map toInt $ words winpart
        your_nums   = map toInt $ words yourpart

countMatch :: ([Int], [Int]) -> Int
countMatch (a, b) = length intersec
  where sa = S.fromList a
        sb = S.fromList b
        intersec = S.intersection sa sb

calPoint :: Int -> Int
calPoint n | n > 0 = 2^(n -1)
           | otherwise = 0

part01 :: [String] -> Int
part01 = sum . map (calPoint . countMatch . splitLine)

mods :: Int -> Int -> [Int]
mods idx n = [(idx+1)..(idx+n)]

changeArr :: Array Int Int -> Int -> Int -> Array Int Int
changeArr arr idx n = arr//[(i, arr!i+arr!idx) | i <- (mods idx n)]

part02:: [String] -> Int
part02 lines = sum (map snd (assocs (foldl f arr source)))
  where source = zip [1..] (map (countMatch. splitLine) lines)
        arr    = listArray (1, length source) (repeat 1)
        f a (i, n) = changeArr a i n     

{--
main = do
  liftM (part01 . lines) (readFile "input") >>= print
--}

main = do
  file <- readFile "input"
  let inp = lines file
  print $ part01 inp
  print $ part02 inp
