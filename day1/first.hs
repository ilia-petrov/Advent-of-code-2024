import Data.List.Split (splitOn)

zipSorted :: Ord a => ([a], [a]) -> [a]
zipSorted ([], []) = []
zipSorted (xs, []) = xs
zipSorted ([], ys) = ys
zipSorted (x:xs, y:ys)
 | x <= y = x : zipSorted (xs, y:ys)
 | otherwise = y : zipSorted (x:xs, ys)

--zipSorted [1,5,7,9] [2,3,6,10]

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = zipSorted (mergeSort (take (length xs `div` 2) xs),
                          mergeSort (drop (length xs `div` 2) xs))

solveSorted :: [Int] -> [Int] -> Int
solveSorted [] [] = 0
solveSorted (x:xs) (y:ys) = abs (x - y) + solveSorted xs ys

solve :: [Int] -> [Int] -> Int
solve xs ys = solveSorted (mergeSort xs) (mergeSort ys)

main = do
    inp <- getContents
    let (xs, ys) = (map ((\num -> read num :: Int) . head . (splitOn "   ")) (lines inp), map ((\num -> read num :: Int) . (!! 1) . (splitOn "   ")) (lines inp)) in
        print $ solve xs ys