import Data.Vector as V
import Prelude as P

convertEdge :: String -> (Int, Int)
convertEdge edge =
    ((fromEnum (edge !! 0) - a) * 26 + (fromEnum (edge !! 1) - a),
    (fromEnum (edge !! 3) - a) * 26 + (fromEnum (edge !! 4) - a)) where
        a = fromEnum 'a'

getMatrix :: [(Int, Int)] -> Vector (Vector Bool)
getMatrix edges =
    fromList $ P.map (\x ->
        fromList $ P.map (\y -> (x, y) `P.elem` edges || (y, x) `P.elem` edges) [0 .. 26 * 26 - 1])
        [0 .. 26 * 26 - 1]

-- t* <- [494..519]

count3Cliques :: Vector (Vector Bool) -> Int
count3Cliques matrix =
    (P.length (P.filter (\(x, y, z) -> (matrix ! x ! z) && (matrix ! y ! z))
        [(x, y, z) | x <- [494 .. 519], y <- [0 .. 26 * 26 - 1], matrix ! x ! y, z <- [0 .. 26 * 26 - 1]]) -
    P.length (P.filter (\(x, y, z) -> (matrix ! x ! z) && (matrix ! y ! z))
        [(x, y, z) | x <- [494 .. 519], y <- [494 .. 519], matrix ! x ! y, z <- [0 .. 26 * 26 - 1]]) +
    P.length (P.filter (\(x, y, z) -> (matrix ! x ! z) && (matrix ! y ! z))
        [(x, y, z) | x <- [494 .. 519], y <- [494 .. 519], matrix ! x ! y, z <- [494 .. 519]])) `div` 2

main = do
    inp <- getContents
    print $ count3Cliques $ getMatrix $ P.map convertEdge $ lines inp