blink :: [Integer] -> [Integer]
blink = concatMap convert

convert ::  Integer -> [Integer]
convert number
  | number == 0 = [1]
  | even (digits number) = [number `div` (10 ^ (digits number `div` 2)), number `mod` (10 ^ (digits number `div` 2))]
  | otherwise = [number * 2024]

digits :: Integer -> Integer
digits number = if number == 0 then 0 else 1 + digits (number `div` 10)

solve :: [Integer] -> Int
solve lst = length (iterate blink lst !! 25)

lstprnt lst = take 15 (iterate blink lst)

main = do
    inp <- getLine
    let lst = map (\wrd -> read wrd :: Integer) (words inp) in
        print $ lstprnt lst