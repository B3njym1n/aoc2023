
parseInp :: [String] -> [[Int]]
parseInp lines = map (map(\w -> read w::Int) . words) lines

differ :: [Int] -> [Int]
differ (x:y:xs) = (y - x) : differ (y:xs)
differ [_] = []

nextValue :: [Int] -> Int
nextValue xs = if all (==0) diff then last xs else (last xs) + (nextValue diff)
  where diff = differ xs

previousValue :: [Int] -> Int
previousValue a@(x:xs) = if all (==0) diff then x else x - (previousValue diff)
  where diff = differ a 

part01 :: [[Int]] -> Int
part01 = sum . (map nextValue)

part02 :: [[Int]] -> Int
part02 = sum . (map previousValue)


main = do
  file <- readFile "input"
  print $ part01 . parseInp . lines $ file
  print $ part02 . parseInp . lines $ file
