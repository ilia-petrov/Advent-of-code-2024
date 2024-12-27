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
generateTowels patts = V.map (`P.elem` converted) $ V.fromList [0 .. 6 ^ 8] where
    converted = P.map convertStr patts

validPattern :: Vector Bool -> String -> [Integer] -> Int -> Integer
validPattern convertedTowels pattern seq pos
 | pos == P.length pattern = seq !! pos
 -- | seq !! pos == 0 = validPattern convertedTowels pattern seq (pos + 1)
 | otherwise = validPattern convertedTowels pattern changedSeq (pos + 1) where
    changedSeq =
        P.take (pos + 1) seq P.++
        P.map (\num -> seq !! (pos + num) +
            if convertedTowels ! convertStr (P.take num reducedPattern) then seq !! pos else 0) [1..8] P.++
        P.drop (pos + 9) seq
    reducedPattern = P.drop pos pattern

solve :: [String] -> [String] -> Integer
solve towels = P.sum . P.map (\pattern -> validPattern convertedTowels pattern (1 : P.take (P.length pattern + 10) [0, 0 ..]) 0) where
    convertedTowels = generateTowels towels

main = do
    inp <- getContents
    let towels = splitOn ", " $ P.head $ lines inp
        patterns = P.drop 2 $ lines inp in
            print $ solve towels patterns
