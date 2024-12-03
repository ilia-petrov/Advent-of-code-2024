import Control.Monad
import Data.List.Split

checkIncr :: [Int] -> Bool
checkIncr [] = True
checkIncr [x] = True
checkIncr (x1:x2:xs) = (x2 - x1) >= 1 && (x2 - x1) <= 3 && checkIncr (x2:xs)

check :: [Int] -> Bool
check lst = foldr (\x res -> res || checkIncr (take (x - 1) lst ++ drop x lst)) (checkIncr lst) [1..length lst] 

solve :: [[Int]] -> Int
solve = foldr (\lst total -> if check lst || check (reverse lst) then total + 1 else total) 0

main = do
    inp <- getContents
    let matr = map (\line -> map read (splitOn " " line) :: [Int]) (lines inp) in
        print $ solve matr