nextDir :: (Int, Int) -> (Int, Int)
nextDir (0, -1) = (1, 0 )
nextDir (1, 0)  = (0, 1 )
nextDir (0, 1)  = (-1, 0)
nextDir (-1, 0) = (0, -1)

movement :: [[Char]] -> (Int, Int) -> (Int, Int) -> [[Char]]
movement grid (x, y) (dx, dy)
 | x < 0 || x >= length (head grid) || y < 0 || y >= length grid = grid
 | grid !! y !! x == '#' = movement grid (x - dx, y - dy) (nextDir (dx, dy))
 | otherwise = movement changedGrid (x + dx, y + dy) (dx, dy) where
    changedGrid = take y grid ++ [take x row ++ "a" ++ drop (x + 1) row] ++ drop (y + 1) grid
    row = grid !! y

findStart :: [[Char]] -> (Int, Int) -> (Int, Int)
findStart [] _ = (-1, -1)
findStart ([] : chs) (x, y) = findStart chs (0, y + 1)
findStart (('^' : _) : _) pos = pos
findStart ((_ : row) : rows) (x, y) = findStart (row : rows) (x + 1, y)

solve :: [[Char]] -> Int
solve grid = length $ filter (== 'a') $ concat $ movement grid (findStart grid (0, 0)) (0, -1)

main = do
    inp <- getContents
    let grid = lines inp in
        print $ solve grid