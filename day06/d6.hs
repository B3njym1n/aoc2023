import qualified Data.List as L

travel :: Int -> Int -> Int
travel charge_t total_t = charge_t * (total_t - charge_t)

-- travel' :: Num -> Num -> Num
travel' charge_t total_t = charge_t * (total_t - charge_t)

-- bruteforce way

bruteforce :: Int -> Int -> Int
bruteforce total_t best_d = length (filter (> best_d) [travel t total_t| t <- [1..total_t]])

-- optimized
answer1 :: Floating a => a -> a -> a
answer1 total_t best_d = (-b + sqrt (b^2 - 4*a*c)) / (2 * a)
  where b = total_t
        a = -1.0
        c = -best_d

answer2 :: Floating a => a -> a -> a
answer2 total_t best_d = (-b - sqrt (b^2 - 4*a*c)) / (2 * a)
  where b = total_t
        a = -1.0
        c = -best_d

wrapped :: Int -> Int -> (Double, Double)
wrapped b c = (ans1, ans2)
  where ans1 = answer1 (fromIntegral b) (fromIntegral c)
        ans2 = answer2 (fromIntegral b) (fromIntegral c)

optimized :: Int -> Int -> Int
optimized b c = upper - lower
  where (a1, a2) = wrapped b c
        upper    = truncate a2
        lower    = truncate a1

toInt :: String -> Int
toInt = read

part01 :: [String] -> Int
part01 xs = foldr (*) 1 (map f pairs)
  where pairs = (drop 1) (L.transpose (map (map toInt . words) xs))
        f [a, b] = optimized a b

part02 :: [String] -> Int
part02 xs = optimized x y
  where (x:y:_) = map ((toInt . foldl (++) "") . (drop 1) . words) xs

main = do
  file <- readFile "input"
  print $ part01 . lines $ file
  print $ part02 . lines $ file
