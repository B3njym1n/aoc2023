import Data.Char
import Data.List
import Data.Array
import Control.Monad

inp2array :: [String] -> Array (Int,Int) Char
inp2array strs = array ((1,1), (rows, cols)) indexed_cols
  where indexed_rows = zip [1..] strs
        indexed_cols = foldl f [] indexed_rows
        f b (i, str) = b ++ [((i, j), ch) | (j, ch) <- zip [1..] str]
        rows = length indexed_rows
        cols = length indexed_cols `div` rows


filterDigits :: [[((Int, Int), Char)]] -> [[((Int, Int), Char)]]
filterDigits = filter (any (isDigit . snd))
    
accumToGroup :: [((Int, Int), Char)] -> [[((Int, Int), Char)]]
accumToGroup =
  groupBy (\a b ->
             ((fst . fst $ a) == (fst . fst $ b))
             &&
             (isDigit . snd $ a)
             &&
             (isDigit . snd $ b)
          )


isAdjcent :: [((Int, Int), Char)] -> Array (Int, Int) Char -> Bool
isAdjcent xs arr = any f xs
  where f = flip cellIsAdjacent arr

cellIsAdjacent :: ((Int, Int), Char) -> Array (Int, Int) Char -> Bool
cellIsAdjacent ((x, y), ch) arr = any f [(i,j) | i <- [x-1,x,x+1], j<-[y-1,y,y+1]]
  where f = isSym arr

isSym :: Array (Int, Int) Char -> (Int, Int) -> Bool
isSym arr (ix, iy)
  | ix < (fst . fst $ bnd) || iy < (fst . fst $ bnd) || ix > (fst . snd $ bnd) || iy > (snd . snd $ bnd) = False
  | otherwise = f arr ix
  where bnd = bounds arr
        f arr ix = (not $ isDigit ch) && (not $ '.' == ch)
        ch = arr!(ix, iy)

isGear :: Array (Int, Int) Char -> (Int, Int) -> Bool
isGear arr (ix, iy)
  | ix < (fst . fst $ bnd) || iy < (fst . fst $ bnd) || ix > (fst . snd $ bnd) || iy > (snd . snd $ bnd) = False
  | otherwise = f arr ix
  where bnd = bounds arr
        f arr ix = '*' == ch
        ch = arr!(ix, iy)

findParts :: [((Int, Int), Char)] -> [[((Int, Int), Char)]]
findParts = filter (any (isDigit . snd)) . groupBy (\a b ->
                                                      ((fst . fst $ a) == (fst . fst $ b))
                                                     &&
                                                     (isDigit . snd $ a)
                                                     &&
                                                     (isDigit . snd $ b)
                                                   )

findEachPartAndGears :: Array (Int, Int) Char -> [((Int, Int), Char)] -> ([(Int, Int)], [((Int, Int), Char)])
findEachPartAndGears arr xs = (gears, xs)
  where
    coords = (map fst xs)
    areas  = map (\(i,j) -> [(x,y) | x <- [i-1, i, i+1], y <- [j-1,j,j+1]]) coords
    area  = foldl (++) [] areas
    gears = nub $ sort $ filter (isGear arr) area

filterPartAndGears :: [([(Int, Int)], [((Int, Int), Char)])] -> [([(Int, Int)], [((Int, Int), Char)])]
filterPartAndGears = filter ((==1) . length . fst)

-- findPartsWithGear :: Array (Int, Int) Char -> [[((Int, Int), Char)]] -> [[((Int, Int), Char)]]
findPartsWithGear arr xs = filter (\xs -> length xs == 2) $ groupBy (\a b -> (fst a) == (fst b)) . sort $ map (\(a,b) -> (head a, b)) (filterPartAndGears (map (findEachPartAndGears arr) xs))

toInt :: [Char] -> Int
toInt = read

chars2int :: [((Int, Int), Char)] -> Int
chars2int = read . map snd

part01 :: [String] -> Int
part01 input = sum $ map chars2int filtered_final
  where arr = inp2array input
        filtered_group  = filterDigits . accumToGroup . assocs $ arr
        filtered_final  = filter fadj filtered_group
        fadj = flip isAdjcent arr

part02 :: [String] -> Int
part02 input = sum (map (\z -> foldl (*) 1 z) parts2ints)
  where arr = inp2array input
        finalParts = findPartsWithGear arr (findParts . assocs $ arr)
        parts2ints = map (\x -> map (\y -> chars2int (snd y)) x) finalParts
        

debug02 input = y
  where arr = inp2array input
        finalParts = findPartsWithGear arr (findParts . assocs $ arr)
        x = map (\x -> map (\y -> chars2int (snd y)) x) finalParts
        y = sum (map (\z -> foldl (*) 1 z) x)

display :: [Int] -> IO ()
display = mapM_ print

solve :: [String] -> [Int]
solve game = [part01 game, part02 game] 

main =
  liftM (solve . lines) (readFile "input") >>= print

  

