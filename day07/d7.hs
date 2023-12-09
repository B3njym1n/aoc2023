import qualified Data.List as L

type Hands = (Int, [Int])

type HandsWithBid = (Hands, Int)

toCard :: Char -> Int
toCard c | c == '2' = 1
         | c == '3' = 2
         | c == '4' = 3
         | c == '5' = 4
         | c == '6' = 5
         | c == '7' = 6
         | c == '8' = 7
         | c == '9' = 8
         | c == 'T' = 9
         | c == 'J' = 10
         | c == 'Q' = 11
         | c == 'K' = 12
         | c == 'A' = 13
         | otherwise = 0

toCard' :: Char -> Int
toCard' c | c == '2' = 1
         | c == '3' = 2
         | c == '4' = 3
         | c == '5' = 4
         | c == '6' = 5
         | c == '7' = 6
         | c == '8' = 7
         | c == '9' = 8
         | c == 'T' = 9
         | c == 'Q' = 11
         | c == 'K' = 12
         | c == 'A' = 13
         | otherwise = 0

getHandtype :: [Int] -> Int
getHandtype xs | length grp == 1 = 6
               | length grp == 2 && any ((==4) . length) grp = 5
               | length grp == 2 && any ((==3) . length) grp = 4
               | length grp == 3 && any ((==3) . length) grp = 3
               | length grp == 3 && any ((==2) . length) grp = 2
               | length grp == 4 = 1
               | otherwise = 0
  where grp = L.group . L.sort $ xs
  

toHand :: String -> Hands
toHand str = (handtype, rest)
  where rest = map toCard str
        handtype = getHandtype rest

sortHands :: Hands -> Hands -> Ordering
sortHands h1 h2 | fst h1 > fst h2 = GT
                | fst h1 < fst h2 = LT
                | fst h1 == fst h2 = compare (snd h1) (snd h2)
                | otherwise = EQ
                
parseThenSort :: [String] -> [(Hands, Int)]
parseThenSort lines = L.sortBy (\a b -> sortHands (fst a) (fst b))(map ((\(x:y:_) -> (toHand x, read y :: Int)) . words) lines)

part01 :: [(Hands, Int)] -> Int
part01 xs = foldl (\b a -> b + (fst a) * (snd $ snd a)) 0 (zip [1..] xs)

parse :: String -> (String, Int)
parse = (\(x:y:_) -> (x, read y ::Int)) . words

mmax :: (Foldable t) => t Int -> Int
mmax xs | null xs = 0
        | otherwise = L.maximum xs

compareHands :: String -> String -> Ordering
compareHands hands1 hands2 = case compare (jh1 + m1) (jh2 + m2) of
  LT -> LT
  GT -> GT
  EQ -> case (jh1 + m1) of
    2 -> compare r2 r1 
    3 -> compare r2 r1
    otherwise -> compare (map toCard' hands1) (map toCard' hands2)
  where jh1 = L.length $ L.filter (=='J') hands1
        jh2 = L.length $ L.filter (=='J') hands2
        m1  = mmax $ map L.length $ L.group $ L.sort $ L.filter (/='J') hands1
        m2  = mmax $ map L.length $ L.group $ L.sort $ L.filter (/='J') hands2
        r1  = L.length $ L.group $ L.sort $ L.filter (/='J') hands1
        r2  = L.length $ L.group $ L.sort $ L.filter (/='J') hands2

part02 :: [String] -> Int
part02 xs = sum $ map (\(i,p) -> i * (snd p)) $ zip [1..] $ L.sortBy (\a b -> compareHands (fst a) (fst b)) (map parse xs)

debug02 :: [String] -> [(String, Int)]
debug02 xs = L.sortBy (\a b -> compareHands (fst a) (fst b)) (map parse xs)

main = do
  file <- readFile "input"
  print $ part01 . parseThenSort . lines $ file
  print $ part02 . lines $ file
  --print $ debug02 . lines $ file
