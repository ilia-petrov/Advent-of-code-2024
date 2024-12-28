import Data.Vector as V
import Prelude as P

nextDir :: (Int, Int) -> (Int, Int)
nextDir (0, -1) = (1, 0 )
nextDir (1, 0)  = (0, 1 )
nextDir (0, 1)  = (-1, 0)
nextDir (-1, 0) = (0, -1)

movement :: Vector (Vector Char) -> (Int, Int) -> (Int, Int) -> Int -> Bool
movement grid (x, y) (dx, dy) counter
 | x < 0 || x >= V.length (V.head grid) || y < 0 || y >= V.length grid = False
 | counter > 130 * 130 = True
 | grid ! y ! x == '#' = movement grid (x - dx, y - dy) (nextDir (dx, dy)) counter
 | otherwise = movement grid (x + dx, y + dy) (dx, dy) (counter + 1)

findStart :: Vector (Vector Char) -> (Int, Int) -> (Int, Int)
findStart grid (x, y)
 | y >= V.length grid = (-1, -1)
 | x >= V.length (V.head grid) = findStart grid (0, y + 1)
 | grid ! y ! x == '^' = (x, y)
 | otherwise = findStart grid (x + 1, y)

isLoop :: Vector (Vector Char) -> (Int, Int) -> (Int, Int) -> Bool
isLoop grid startPos (noX, noY)
 | startPos == (noX, noY) = False
 | otherwise = movement changedGrid startPos (0, -1) 0 where
    changedGrid = V.take noY grid V.++ fromList [V.take noX row V.++ fromList "#" V.++ V.drop (noX + 1) row] V.++ V.drop (noY + 1) grid
    row = grid ! noY

solve :: Vector (Vector Char) -> Int
solve grid = P.length $ P.filter id $ [isLoop grid startPos (x, y) |
                                     x <- [0 .. V.length (V.head grid) - 1], y <- [0 .. V.length grid - 1]] where
        startPos = findStart grid (0, 0)

main = do
    inp <- getContents
    let grid = lines inp in
        print $ solve $ V.map fromList $ fromList grid