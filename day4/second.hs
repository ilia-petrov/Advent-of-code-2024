--text word
foundWord :: [String] -> [String] -> Int
foundWord ((t11:_:t13:_):(_:t22:_):(t31:_:t33:_):_) ((w11:_:w13:_):(_:w22:_):(w31:_:w33:_):_) =
    if t11 == w11 && t13 == w13 && t22 == w22 && t31 == w31 && t33 == w33 then 1 else 0
foundWord _ _ = 0

allInRow :: [String] -> [String] -> Int
allInRow ([]:_) _ = 0
allInRow matr word = foundWord matr word + allInRow (map tail matr) word

allInMatr :: [String] -> [String] -> Int
allInMatr [] _ = 0
allInMatr matr word = allInRow matr word + allInMatr (tail matr) word

solve :: [String] -> Int
solve matr =
    allInMatr matr ["M S", " A ", "M S"] +
    allInMatr matr ["M M", " A ", "S S"] +
    allInMatr matr ["S M", " A ", "S M"] +
    allInMatr matr ["S S", " A ", "M M"]

main = do
    inp <- getContents
    print $ solve $ lines inp