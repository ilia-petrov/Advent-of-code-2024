import Data.List.Split

solveOne :: (Int, Int, Int) -> (Int, Int, Int) -> Int
solveOne (a1, b1, c1) (a2, b2, c2) =
    if   x >= 0 && y >= 0 &&
         x * a1 + y * b1 == c1 &&
         x * a2 + y * b2 == c2
    then x * 3 + y
    else 0
    where
        x = (c1 * b2 - c2 * b1) `div` (a1 * b2 - a2 * b1)
        y = (c1 * a2 - c2 * a1) `div` (b1 * a2 - b2 * a1)

solve :: [((Int, Int, Int), (Int, Int, Int))] -> Int
solve = sum . map (uncurry solveOne)

main = do
    inp <- getContents
    let tokens = splitOn "\n\n" inp
        solvable = [((dx1, dx2, tarx + 10000000000000), (dy1, dy2, tary + 10000000000000)) |
            parts <- map (splitOneOf "+=,\n") tokens,
            dx1 <- [read (parts !! 1) :: Int],
            dx2 <- [read (parts !! 5) :: Int],
            tarx <- [read (parts !! 9) :: Int],
            dy1 <- [read (parts !! 3) :: Int],
            dy2 <- [read (parts !! 7) :: Int],
            tary <- [read (parts !! 11) :: Int]] in
                print $ solve solvable