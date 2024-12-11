import Data.List.Split (splitOn)
import Data.Maybe

foundRule :: [(Int, Int)] -> Int -> Int -> Bool
foundRule rules x y = foldr (\(r1, r2) res -> (r1 == x && r2 == y) || res) False rules

checkLine :: [(Int, Int)] -> [Int] -> Bool
checkLine _ [_] = True
checkLine rules (x:y:xs) = not (foundRule rules y x) && checkLine rules (y:xs)

getMid :: [a] -> a
getMid lst = lst !! (length lst `div` 2)

findNext :: [(Int, Int)] -> [Int] -> Int -> [Int]
findNext _ [] _ = []
findNext rules (x:xs) y = if foundRule rules y x then x : findNext rules xs y else findNext rules xs y

ordAfter :: [(Int, Int)] -> [Int] -> Int -> [Int]
ordAfter rules xs x = x : foldr (\path res -> if length path > length res then path else res)
                            [] (map (ordAfter rules xs) (findNext rules xs x))

ordBefore :: [(Int, Int)] -> [Int] -> Int -> [Int]
ordBefore rules xs x = reverse $ ordAfter (map (\(a, b) -> (b, a)) rules) xs x

ord :: [(Int, Int)] -> [Int] -> [Int]
ord rules (x:xs) = ordBefore rules xs x ++ tail (ordAfter rules xs x)

solveLine :: [(Int, Int)] -> [Int] -> Int
solveLine rules lst = if checkLine rules lst then 0 else getMid $ ord rules lst

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