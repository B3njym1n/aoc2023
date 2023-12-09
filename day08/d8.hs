import qualified Data.Map as M

parse :: [String] -> (String, [(String,(String,String))])
parse (x:y:rest) = (instruction, mappings)
  where instruction = x
        mappings = map f rest
        f line   = (take 3 line, (take 3 $ drop 7 line, take 3 $ drop 12 line))

step :: String -> M.Map String (String,String) -> String -> Int
step (x:xs) mapping key | x == 'L' = if left == "ZZZ" then 1 else 1 + step xs mapping left
                        | x == 'R' = if right == "ZZZ" then 1 else 1 + step xs mapping right
                        where (left, right) = (M.!) mapping key

part01 :: (String, [(String,(String,String))]) -> Int
part01 (inst, mapping) = step (concat (repeat inst)) (M.fromList mapping) "AAA"

endsA :: String -> Bool
endsA = (=='A') . last

endsZ :: String -> Bool
endsZ = (=='Z') . last

startkeys :: [(String,(String,String))] -> [String]
startkeys = filter endsA . map fst

allEndZ :: [String] -> Bool
allEndZ = all endsZ

step2 :: String -> M.Map String (String,String) -> [String] -> Int
step2 (x:xs) mapping keys = case allEndZ newkeys of
  True -> 1
  False -> 1 + step2 xs mapping newkeys
  where newkeys = nextKeys keys (ch2dir x) mapping

data Direction = Leftward | Rightward deriving (Show, Eq)

ch2dir :: Char -> Direction
ch2dir ch | ch == 'L' = Leftward
          | ch == 'R' = Rightward

nextKeys :: [String] -> Direction -> M.Map String (String,String) -> [String]
nextKeys oldkeys direction mapping | direction == Leftward = map (fst . (M.!) mapping) oldkeys
                                   | direction == Rightward = map (snd . (M.!) mapping) oldkeys
{-- too slow
part02 :: (String, [(String,(String,String))]) -> Int
part02 (inst, mapping) = step2 (concat (repeat inst)) mm (startkeys mapping)
  where mm = M.fromList mapping
--}

step' :: String -> M.Map String (String,String) -> String -> Int
step' (x:xs) mapping key | x == 'L' = if endsZ left then 1 else 1 + step' xs mapping left
                         | x == 'R' = if endsZ right then 1 else 1 + step' xs mapping right
  where (left, right) = (M.!) mapping key

debug2 (inst, mapping) = map (f instrloop mm) (startkeys mapping)
  where f inst mm k = step' inst mm k
        mm = M.fromList mapping
        instrloop = concat (repeat inst)

lcmOfList :: [Int] -> Int
lcmOfList = foldl (\b a -> (a * b) `div` (gcd a b)) 1

part02 :: (String, [(String,(String,String))]) -> Int
part02 (inst, mapping) = lcmOfList counts
  where counts =  map (f instrloop mm) (startkeys mapping)
        f inst mm k = step' inst mm k
        mm = M.fromList mapping
        instrloop = concat (repeat inst)

main = do
  file <- readFile "input"
  print $ part01 . parse $ lines file
  --print $ (startkeys . snd) . parse $ lines file
  print $ part02 . parse $ lines file
