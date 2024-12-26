import Data.List.Split (splitOneOf)
simPos :: (Int, Int) -> (Int, Int) -> Int -> Int -> Int -> (Int, Int)
simPos (px, py) (vx, vy) times maxX maxY =
    ((px + vx * times) `mod` maxX, (py + vy * times) `mod` maxY)

solve :: [((Int, Int), (Int, Int))] -> Int -> Int -> Int -> String
solve robots times maxX maxY =
    unlines [map (\ p -> if p `elem` poss then 'X' else '.')
       ([(x, y) | y <- [0 .. 100]]) |
       x <- [0 .. 102]] where
        poss = map (\(rP, rV) -> simPos rP rV times maxX maxY) robots

main = do
    inp <- getContents
    let lns = lines inp
        solvables = [((px, py), (vx, vy)) |
            parts <- map (splitOneOf "=, ") lns,
            px <- [read (parts !! 1) :: Int],
            py <- [read (parts !! 2) :: Int],
            vx <- [read (parts !! 4) :: Int],
            vy <- [read (parts !! 5) :: Int]] in
                putStr $ unlines $ map (\x -> (solve solvables x 101 103) ++ show x ++ "\n") [x | x <- [62 * 101 + 23 .. 63 * 101 + 23]]