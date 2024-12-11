import Data.List (nub)
addLetter :: Char -> Int -> Int -> [(Char, [(Int, Int)])] -> [(Char, [(Int, Int)])]
addLetter ch x y [] = [(ch, [(x, y)])]
addLetter ch x y ((entryCh, entryPoss) : entries)
 | ch == entryCh = (entryCh, (x, y) : entryPoss) : entries
 | otherwise     = (entryCh, entryPoss) : addLetter ch x y entries

generateDictionary :: [String] -> Int -> Int -> [(Char, [(Int, Int)])] -> [(Char, [(Int, Int)])]
generateDictionary [] _ _ res = res
generateDictionary (['.']   :strs) x y res = generateDictionary strs 0 (y + 1) res
generateDictionary ([ch]    :strs) x y res = generateDictionary strs 0 (y + 1) (addLetter ch x y res)
generateDictionary (('.':ln):strs) x y res = generateDictionary (ln:strs) (x + 1) y res
generateDictionary ((ch :ln):strs) x y res = generateDictionary (ln:strs) (x + 1) y (addLetter ch x y res)

genLn :: (Int, Int) -> (Int, Int) -> Int -> Int -> [(Int, Int)]
genLn (x1, y1) (x2, y2) n m = [(x2 + i * dx `div` g, y2 + i * dy `div` g) |
                                i <- [0..(max m n)],
                                x2 + i * (dx `div` g) < m && x2 + i * (dx `div` g) >= 0 &&
                                y2 + i * (dy `div` g) < n && y2 + i * (dy `div` g) >= 0] where
                                    dx = x2 - x1
                                    dy = y2 - y1
                                    g = gcd dx dy

solveDictionary :: [(Char, [(Int, Int)])] -> Int -> Int -> Int
solveDictionary dictionary n m =
    length $
    filter (\(x, y) -> x >= 0 && x < m && y >= 0 && y < n) $
    nub $
    concatMap (\entry -> concat [genLn (x1, y1) (x2, y2) n m |
                          (x1, y1) <- snd entry, (x2, y2) <- snd entry, x1 /= x2 || y1 /= y2]) dictionary

solve :: [String] -> Int
solve lns = solveDictionary (generateDictionary lns 0 0 []) (length lns) (length $ head lns)

main = do
    inp <- getContents
    print $ solve $ lines inp