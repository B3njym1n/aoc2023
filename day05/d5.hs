import qualified Data.List as L
import qualified Data.Map.Strict as M

isNonEmptyLine :: String -> Bool
isNonEmptyLine = (>0) . length

toBlock :: [String] -> [[String]]
toBlock = L.groupBy (\a b ->  (isNonEmptyLine a) && (isNonEmptyLine b))

toInt :: String -> Int
toInt = read

getSeedList :: String -> [Int]
getSeedList = (map toInt) . words . drop 7

data Bound = Bound {
  src :: Int,
  dest :: Int,
  rng :: Int
  } deriving (Eq, Show)

lineToBound :: String -> Bound
lineToBound line = Bound src dest rng
  where [dest, src, rng] = map toInt $ words line

blist :: [String] -> [Bound]
blist = foldr f []
  where f a b = (lineToBound a) : b

perEachMap :: [String] -> [Bound]
perEachMap (x:xs) = blist xs

checkBound :: Int -> Bound  -> Bool
checkBound n b = lower <= n && upper >= n
  where lower = src b
        upper = (rng b + src b) - 1

findBlist :: [Bound] -> Int  -> Int
findBlist b n = case L.find (checkBound n) b of
  Just b -> (dest b) + (n - src b)
  Nothing -> n

checkAllBounds :: Int -> [[Bound]] -> Int
checkAllBounds n b = foldl f n b
  where f b a = findBlist a b
part01 :: [Int] -> [[Bound]] -> Int
part01 seeds mappings = minimum (map (flip checkAllBounds mappings) seeds)

pairSeeds :: [Int] -> [Int]
pairSeeds (x:(y:res)) = [x..(x+y-1)] ++ (pairSeeds res)
pairSeeds [] = []

part02 :: [Int] -> [[Bound]] -> Int
part02 seeds mappings = minimum (map (flip checkAllBounds mappings) (pairSeeds seeds))

{-- experiments --}

data Mapf k v = Mapf {
  runMapf :: k -> v
  }

createMapf :: [Int] -> [Int] -> Mapf Int (Maybe (Int, Int))
createMapf xs ys = Mapf (\inp -> L.find ((==inp) . fst) (zip xs ys) )

createMapf2 :: Int -> Int -> Int -> Mapf Int Int
createMapf2 dst src rng = Mapf (\inp -> if (src <= inp&& inp < src+rng)
                                 then dst + inp - src
                                 else inp
                               )

main = do
  file <- readFile "input"
  let input = lines $ file
  let seeds = getSeedList . head $ input
  let mapping = map perEachMap (filter ((>1) . length) . toBlock . (drop 1) $ input)
  print (part01 seeds mapping)
  -- too slow print (part02 seeds mapping)
  
