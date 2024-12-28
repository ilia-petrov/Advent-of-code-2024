import Data.Vector as V
import Prelude as P

convertGrid :: [[Char]] -> Vector (Vector Char)
convertGrid grid =
    fromList [fromList (P.take (width + 2) ['a', 'a' ..])] V.++
    V.map (\row -> fromList ('a' : row P.++ "a")) (fromList grid) V.++
    fromList [fromList (P.take (width + 2) ['a', 'a' ..])] where
        width = P.length $ P.head grid

bfs :: Vector (Vector Char) -> Char -> [(Int, Int)] -> Vector (Vector Bool) -> Int -> Int -> (Vector (Vector Bool), Int, Int)
bfs _ _ [] used area perimeter = (used, area, perimeter)
bfs grid ch ((x, y) : poss) used area perimeter
 | used ! y ! x || grid ! y ! x /= ch = bfs grid ch poss used area perimeter
 | otherwise = bfs grid ch changedPoss changedUsed (area + 1) (perimeter + cntNbrs grid x y) where
    changedPoss = poss P.++ [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]
    changedUsed = 
        V.take y used V.++ 
        fromList [V.take x row V.++ fromList [True] V.++ V.drop (x + 1) row]
        V.++ V.drop (y + 1) used
    row = used ! y

cntNbrs :: Vector (Vector Char) -> Int -> Int -> Int
cntNbrs grid x y =
    P.length $ P.filter id
        [grid ! y ! x /= grid ! y ! (x + 1),
         grid ! y ! x /= grid ! y ! (x - 1),
         grid ! y ! x /= grid ! (y + 1) ! x,
         grid ! y ! x /= grid ! (y - 1) ! x]

solve :: Vector (Vector Char) -> Vector (Vector Bool) -> Int -> Int -> Int
solve grid used x y
 | y >= V.length grid - 1 = 0
 | x >= V.length (V.head grid) - 1 = solve grid used 1 (y + 1)
 | otherwise = p * a + solve grid newUsed (x + 1) y where
    (newUsed, a, p) = bfs grid (grid ! y ! x) [(x, y)] used 0 0

main = do
    inp <- getContents
    let grid = lines inp in
        print $ solve (convertGrid grid) (V.replicate (P.length grid + 2) (V.replicate (P.length (P.head grid) + 2) False)) 1 1