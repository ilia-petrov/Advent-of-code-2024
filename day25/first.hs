import Data.List.Split (splitOn)
transpone :: [[a]] -> [[a]]
transpone [] = []
transpone ([]:_) = []
transpone matrix = map head matrix : transpone (map tail matrix)

addKeyLock :: ([[Int]], [[Int]]) -> [[Char]] -> ([[Int]], [[Int]])
addKeyLock (keys, locks) keylock
 | head (head keylock) == '#' = (keys, map (length . filter (== '#')) keylock : locks)
 | otherwise = (map (length . filter (== '#')) keylock : keys, locks)

addAll :: [[[Char]]] -> ([[Int]], [[Int]]) -> ([[Int]], [[Int]])
addAll keylocks res = foldl addKeyLock res keylocks

addPair :: [Int] -> [Int] -> [Int]
addPair [] [] = []
addPair (x : xs) (y : ys) = (x + y) : addPair xs ys

cntKeys :: [[Int]] -> [[Int]] -> Int
cntKeys keys locks = sum $ map (\key -> length $ filter (all (<= 7) . addPair key) locks) keys

solve :: [[[Char]]] -> Int
solve inps = uncurry cntKeys $ addAll (map transpone inps) ([], [])

main = do
    inp <- getContents
    let inps = map (splitOn "\n") $ splitOn "\n\n" inp in
        print $ solve inps