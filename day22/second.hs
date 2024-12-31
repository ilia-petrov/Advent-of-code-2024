import Data.Bits

generateUpTo :: Int -> Int -> [Int]
generateUpTo res 0 = [res]
generateUpTo num n = num : generateUpTo third (n - 1) where
    first  = (num `xor` (num * 64)) `mod` 16777216
    second = (first `xor` (first `div` 32)) `mod` 16777216
    third  = (second `xor` (second * 2048)) `mod` 16777216

data BSTree a = BSTree {val :: a, left :: BSTree a, right :: BSTree a} | Empty
newtype Map k v = Map (BSTree (k, v))

addSeq :: Map (Int, Int, Int, Int) Int -> (Int, Int, Int, Int) -> Int -> Map (Int, Int, Int, Int) Int
addSeq Empty seq last = (seq, last) Empty Empty 

checkSeq :: [Int] -> (Int, Int, Int, Int) -> Int
checkSeq (p1:p2:p3:p4:p5:ps) (s1, s2, s3, s4)
 | s1 + s2 > 9 || s1 + s2 < -9 || s2 + s3 > 9 || s2 + s3 < -9 || s3 + s4 > 9 || s3 + s4 < -9 || s1 + s2 + s3 > 9 || s1 + s2 + s3 < -9 || s2 + s3 + s4 > 9 || s2 + s3 + s4 < -9 || s1 + s2 + s3 + s4 > 9 || s1 + s2 + s3 + s4 < -9 = 0
 | p2 - p1 == s1 && p3 - p2 == s2 && p4 - p3 == s3 && p5 - p4 == s4 = p5
 | otherwise = checkSeq (p2:p3:p4:p5:ps) (s1, s2, s3, s4)
checkSeq _ _ = 0

solve :: [Int] -> Int
solve lst = maximum $ [(\ seq
                          -> sum (map (`checkSeq` seq) prices))
                         (s1, s2, s3, s4) |
                         s1 <- [-9 .. 9],
                         s2 <- [-9 .. 9],
                         s3 <- [-9 .. 9],
                         s4 <- [-9 .. 9]] where 
                            prices = map (map (`mod` 10) . (`generateUpTo` 2000)) lst

-- >>> map (`checkSeq` (-3, 6, -1, -1)) $ map (map (`mod` 10) . (`generateUpTo` 2000)) [123, 1, 2, 3]
-- [4,0,6,0]

-- >>> solve [1, 2, 3, 2024]
-- 23
main = do
    inp <- getContents
    let lst = map (\num -> read num :: Int) $ words inp in
        print $ solve lst
