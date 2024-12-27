import Data.Vector as V
import Prelude as P
import Data.List.Split (splitOn)

convertCh :: Char -> Int
convertCh 'w' = 5
convertCh 'u' = 1
convertCh 'b' = 2
convertCh 'r' = 3
convertCh 'g' = 4

convertStr :: String -> Int
convertStr "" = 0
convertStr (ch:chs) = convertStr chs * 6 + convertCh ch

generateTowels :: [String] -> Vector Bool
generateTowels patts = V.map (`P.elem` converted) $ V.fromList [0 .. 6 ^ 8 - 1] where
    converted = P.map convertStr patts

validPattern :: Vector Bool -> String -> [Bool] -> Int -> Bool
validPattern convertedTowels pattern seq pos
 | pos == P.length pattern = seq !! pos
 | not (seq !! pos) = validPattern convertedTowels pattern seq (pos + 1)
 | otherwise = validPattern convertedTowels pattern changedSeq (pos + 1) where
    changedSeq =
        P.take (pos + 1) seq P.++
        P.map (\num -> seq !! (pos + num) || (convertedTowels ! convertStr (P.take num reducedPattern))) [1..8] P.++
        P.drop (pos + 9) seq
    reducedPattern = P.drop pos pattern

solve :: [String] -> [String] -> Int
solve towels = P.length . P.filter (\pattern -> validPattern convertedTowels pattern (True : P.take (P.length pattern + 10) [False, False ..]) 0) where
    convertedTowels = generateTowels towels

main = do
    inp <- getContents
    let towels = splitOn ", " $ P.head $ lines inp
        patterns = P.drop 2 $ lines inp in
            print $ solve towels patterns
