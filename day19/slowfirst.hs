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

validPattern :: Vector Bool -> String -> Bool
validPattern _ "" = True
validPattern convertedTowels pattern =
    P.foldr (\num res ->
        if   convertedTowels ! convertStr (P.take num pattern)
        then res || validPattern convertedTowels (P.drop num pattern)
        else res) False [1..8]

solve :: [String] -> [String] -> Int
solve towels = P.length . P.filter (generateTowels towels `validPattern`)

main = do
    inp <- getContents
    let towels = splitOn ", " $ P.head $ lines inp
        patterns = P.drop 2 $ lines inp in
            print $ solve towels patterns
