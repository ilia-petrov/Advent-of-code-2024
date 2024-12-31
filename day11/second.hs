hmGenerated :: Int -> Integer -> Integer
hmGenerated 0 _ = 1
hmGenerated lvl number
 | number == 0 = hmGenerated (lvl - 1) 1
 | even (digits number) = hmGenerated (lvl - 1) (number `div` (10 ^ (digits number `div` 2))) +
                          hmGenerated (lvl - 1) (number `mod` (10 ^ (digits number `div` 2)))
 | otherwise = hmGenerated (lvl - 1) (number * 2024)

digits :: Integer -> Integer
digits number = if number == 0 then 0 else 1 + digits (number `div` 10)

solve :: [Integer] -> Integer
solve = sum . map (hmGenerated 75)

lstprnt = map (hmGenerated 75)

main = do
    inp <- getLine
    let lst = map (\wrd -> read wrd :: Integer) (words inp) in
        print $ lstprnt lst