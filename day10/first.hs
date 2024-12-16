import Data.Vector as Vec
import Prelude as Pre
import Data.List (nub)

reachables :: Vector (Vector Int) -> Int -> Int -> Bool -> [(Int, Int)]
reachables matrix x y False = []
reachables matrix x y True  =
    if el == 9 then [(x, y)] else
        reachables matrix (x + 1) y (x + 1 < Vec.length (Vec.head matrix) && el + 1 == matrix ! y ! (x + 1)) Pre.++
        reachables matrix (x - 1) y (x - 1 >= 0                           && el + 1 == matrix ! y ! (x - 1)) Pre.++
        reachables matrix x (y + 1) (y + 1 < Vec.length matrix            && el + 1 == matrix ! (y + 1) ! x) Pre.++
        reachables matrix x (y - 1) (y - 1 >= 0                           && el + 1 == matrix ! (y - 1) ! x) where
            el = matrix ! y ! x

score :: Vector (Vector Int) -> Int -> Int -> Int
score matrix x y
 | y >= Vec.length matrix = 0
 | x >= Vec.length (Vec.head matrix) = score matrix 0 (y + 1)
 | matrix ! y ! x /= 0 = score matrix (x + 1) y
 | otherwise = Pre.length (nub $ reachables matrix x y True) + score matrix (x + 1) y

generateRow :: [Int] -> Vector Int
generateRow [] = empty
generateRow (x:xs) = cons x (generateRow xs)

generateMatrix :: [[Int]] -> Vector (Vector Int)
generateMatrix [] = empty
generateMatrix (xs:xss) = cons (generateRow xs) (generateMatrix xss)

main = do
    inp <- getContents
    let matr = Pre.map (Pre.map (\ch -> read [ch] :: Int)) (lines inp) in
        print $ score (generateMatrix matr) 0 0