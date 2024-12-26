import Data.List.Split (splitOneOf)
simPos :: (Int, Int) -> (Int, Int) -> Int -> Int -> Int -> (Int, Int)
simPos (px, py) (vx, vy) times maxX maxY =
    ((px + vx * times) `mod` maxX, (py + vy * times) `mod` maxY)

solve :: [((Int, Int), (Int, Int))] -> Int -> Int -> Int -> Int
solve robots times maxX maxY =
    first * second * third * fourth where
        first  = length $ filter (\(x, y) -> x < maxX `div` 2 && y < maxY `div` 2) pos
        second = length $ filter (\(x, y) -> x < maxX `div` 2 && y > maxY `div` 2) pos
        third  = length $ filter (\(x, y) -> x > maxX `div` 2 && y < maxY `div` 2) pos
        fourth = length $ filter (\(x, y) -> x > maxX `div` 2 && y > maxY `div` 2) pos
        pos = map (\(rP, rV) -> simPos rP rV times maxX maxY) robots

main = do
    inp <- getContents
    let lns = lines inp
        solvables = [((px, py), (vx, vy)) |
            parts <- map (splitOneOf "=, ") lns,
            px <- [read (parts !! 1) :: Int],
            py <- [read (parts !! 2) :: Int],
            vx <- [read (parts !! 4) :: Int],
            vy <- [read (parts !! 5) :: Int]] in
                print $ solve solvables 100 101 103