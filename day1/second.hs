solve:: [Integer] -> [Integer] -> Integer
solve xs ys = foldr (\x sum ->sum + x * foldr (\y times -> if x == y then times + 1 else times) 0 ys) 0 xs