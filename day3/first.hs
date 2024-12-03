--import Distribution.Compat.Prelude (readMaybe)
import Data.Maybe
import Data.List.Split
import Text.Read

parseInput :: String -> [(Int, Int)]
parseInput "" = []
parseInput ('m':'u':'l':'(':str) = 
    let atoms = splitOn "," str
        first = readMaybe (head atoms) :: Maybe Int in
            if isJust first then
                let sec = readMaybe (head (splitOn ")" (atoms !! 1))) :: Maybe Int in
                    if isJust sec then (fromJust first, fromJust sec) : parseInput str else parseInput str
            else parseInput str
parseInput (_:str) = parseInput str

solve :: String -> Int
solve str = foldr (+) 0 $ map (\(x, y) -> x * y) $ parseInput str

main = do
    inp <- getContents
    print $ solve inp