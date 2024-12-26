import Data.Bits

generateNth :: Int -> Int -> Int
generateNth res 0 = res
generateNth num n = generateNth third (n - 1) where
    first  = (num `xor` (num * 64)) `mod` 16777216
    second = (first `xor` (first `div` 32)) `mod` 16777216
    third  = (second `xor` (second * 2048)) `mod` 16777216

solve :: [Int] -> Int
solve = sum . map (`generateNth` 2000)

main = do
    inp <- getContents
    let lst = map (\num -> read num :: Int) $ words inp in
        print $ solve lst