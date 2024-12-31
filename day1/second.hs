import Data.List.Split (splitOn)
solve:: [Int] -> [Int] -> Int
solve xs ys = foldr (\x sum ->sum + x * foldr (\y times -> if x == y then times + 1 else times) 0 ys) 0 xs

main = do
    inp <- getContents
    let (xs, ys) = (map ((\num -> read num :: Int) . head . (splitOn "   ")) (lines inp), map ((\num -> read num :: Int) . (!! 1) . (splitOn "   ")) (lines inp)) in
        print $ solve xs ys