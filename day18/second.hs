import Data.List.Split (splitOn)
bfs :: [[Int]] -> [(Int, Int, Int)] -> Int -> Int -> [[Int]]
bfs grid [] _ _ = grid
bfs grid ((x, y, val) : poss) maxX maxY
 | x < 0 || x > maxX ||
   y < 0 || y > maxY ||
   grid !! y !! x /= -1 = bfs grid poss maxX maxY
 | otherwise = bfs changedGrid
        (poss ++ [(x + 1, y, val + 1), (x - 1, y, val + 1), (x, y + 1, val + 1), (x, y - 1, val + 1)]) maxX maxY where
    changedGrid = take y grid ++ [take x (grid !! y) ++ [val] ++ drop (x + 1) (grid !! y)] ++ drop (y + 1) grid

generateGrid :: [(Int, Int)] -> Int -> Int -> [[Int]]
generateGrid bytes maxX maxY = [map (\ (x, y) -> if (x, y) `elem` bytes then -2 else -1)
                                  ([(x, y) | x <- [0 .. maxX]]) |
                                  y <- [0 .. maxY]]


reachable :: [(Int, Int)] -> Int -> Int -> Bool
reachable bytes maxX maxY = (bfsGrid !! maxY !! maxX) /= -1 where
    bfsGrid = bfs (generateGrid bytes maxX maxY) [(0,0,0)] maxX maxY

solve :: [(Int, Int)] -> Int -> Int -> Int -> Int -> (Int, Int)
solve bytes l r maxX maxY
 | l == r - 1 = bytes !! r
 | otherwise = if reachable (take (mid + 1) bytes) maxX maxY then solve bytes mid r maxX maxY else solve bytes l mid maxX maxY where
    mid = (l + r) `div` 2
main = do
    inp <- getContents
    let bytes = map ((\(x : y : _) -> (read x :: Int, read y :: Int)) . splitOn ",") (lines inp) in
        print $ solve bytes 0 (length bytes) 70 70
