import Data.Char

findFirst :: [Char] -> Int
findFirst (x:xs) | isDigit x = digitToInt x
                 | otherwise = findFirst xs

findLast :: [Char] -> Int
findLast xs | isDigit lastC = digitToInt lastC
            | otherwise     = findLast (init xs)
            where lastC = last xs

parseLine :: [Char] -> (Int, Int)
parseLine line = (fstD, lastD)
  where fstD = findFirst line
        lastD = findLast line

combineToNum :: (Int, Int) -> Int
combineToNum (a,b) = a* 10 + b

part01 :: [[Char]] -> Int
part01 = sum . map (combineToNum . parseLine)

parseLine2 :: [Char] -> (Int, Int)
parseLine2 line = (fstD, lastD)
  where fstD = findFirst2 line
        lastD = findLast2 line

findFirst2 :: [Char] -> Int
findFirst2 s@(x:xs) | isDigit x = digitToInt x
                    | take 3 s == "one" = 1
                    | take 3 s == "two" = 2
                    | take 5 s == "three" = 3
                    | take 4 s == "four" = 4
                    | take 4 s == "five" = 5
                    | take 3 s == "six" = 6
                    | take 5 s == "seven" = 7
                    | take 5 s == "eight" = 8
                    | take 4 s == "nine" = 9
                    | otherwise = findFirst2 xs

findLast2 :: [Char] -> Int
findLast2 s = f2 (reverse s)
  where f2 s'@(x:xs) | isDigit x = digitToInt x
                     | take 3 s' == "eno" = 1
                     | take 3 s' == "owt" = 2
                     | take 5 s' == "eerht" = 3
                     | take 4 s' == "ruof" = 4
                     | take 4 s' == "evif" = 5
                     | take 3 s' == "xis" = 6
                     | take 5 s' == "neves" = 7
                     | take 5 s' == "thgie" = 8
                     | take 4 s' == "enin" = 9
                     | otherwise = f2 xs

part02 :: [[Char]] -> Int
part02 = sum . map (combineToNum . parseLine2)

solve :: [[Char]] -> (Int, Int)
solve inp = (part01 inp, part02 inp)

main = readFile "input" >>= print . solve . lines
