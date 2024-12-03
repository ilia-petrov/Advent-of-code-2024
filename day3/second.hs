--import Distribution.Compat.Prelude (readMaybe)
import Data.Maybe
import Data.List.Split
import Text.Read

parseInput :: Bool -> String -> [(Int, Int)]
parseInput _ "" = []
parseInput _ ('d':'o':'(':')':str)              = parseInput True   str
parseInput _ ('d':'o':'n':'\'':'t':'(':')':str) = parseInput False  str
parseInput False (_:str)                        = parseInput False str
parseInput True ('m':'u':'l':'(':str) = 
    let atoms = splitOn "," str
        first = readMaybe (head atoms) :: Maybe Int in
            if isJust first then
                let sec = readMaybe (head (splitOn ")" (atoms !! 1))) :: Maybe Int in
                    if isJust sec then (fromJust first, fromJust sec) : parseInput True str else parseInput True str
            else parseInput True str
parseInput should (_:str) = parseInput should str

solve :: String -> Int
solve str = foldr (+) 0 $ map (\(x, y) -> x * y) $ parseInput True str

main = do
    inp <- getContents
    print $ solve inp