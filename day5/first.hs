import Data.List.Split (splitOn)

checkLine :: [(Int, Int)] -> [Int] -> Bool
checkLine _ [_] = True
checkLine rules (x:y:xs) = foldr (\(r1, r2) res -> (r1 /= y || r2 /= x) && res) True rules && checkLine rules (y:xs)

getMid :: [a] -> a
getMid lst = lst !! (length lst `div` 2)

solveLine :: [(Int, Int)] -> [Int] -> Int
solveLine rules lst = if checkLine rules lst then getMid lst else 0

solve :: [(Int, Int)] -> [[Int]] -> Int
solve rules = foldr (\lst res -> res + solveLine rules lst) 0

main = do
    inp <- getContents
    let lns = lines inp
        ruleLns = filter (elem '|') lns
        lstLns = filter (\ln -> '|' `notElem` ln && not (null ln)) lns
        rules = foldr (:) [] (map (\ln -> (read (splitOn "|" ln !! 0) :: Int, read (splitOn "|" ln !! 1) :: Int)) ruleLns)
        lsts = map (map (\x -> read x :: Int) . splitOn ",") lstLns in
            print $ solve rules lsts