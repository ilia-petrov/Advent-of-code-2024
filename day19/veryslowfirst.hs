import Data.List.Split (splitOn)
maches :: String -> String -> Bool
maches "" _ = True
maches _ "" = False
maches (w:ws) (t:ts) = t == w && maches ws ts

validPattern :: [String] -> String -> Bool
validPattern _ "" = True
validPattern towels pattern =
    foldr (\towel res ->
            if   maches towel pattern
            then res || validPattern towels (drop (length towel) pattern)
            else res) False towels

solve :: [String] -> [String] -> Int
solve towels = length . filter (towels `validPattern`)

main = do
    inp <- getContents
    let towels = splitOn ", " $ head $ lines inp
        patterns = drop 2 $ lines inp in
            print $ solve towels patterns