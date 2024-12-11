import Data.List.Split (wordsBy)
checkVal :: [Int] -> Int -> Int -> Bool
checkVal [] res tar = res == tar
checkVal (x:xs) res tar = checkVal xs (res + x) tar || checkVal xs (res * x) tar

solveLn :: [Int] -> Int -> Int
solveLn (x:xs) tar = if checkVal xs x tar then tar else 0

solve :: [(Int, [Int])] -> Int
solve exprs = sum $ map (\expr -> solveLn (snd expr) (fst expr)) exprs

main = do
    inp <- getContents
    let lns = map (map (\num -> read num ::Int) . wordsBy (`elem` " :")) (lines inp) in
        print $ solve $ map (\ln -> (head ln, tail ln)) lns