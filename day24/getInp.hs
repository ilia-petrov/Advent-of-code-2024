import Data.List.Split (splitOn)
import Data.Char (toLower)

parseInit :: String -> String
parseInit init =
    concatMap ((\tokens -> "f\"" ++ head tokens ++ "\" = " ++ (tokens !! 1) ++ "\n") . splitOn ": ") (splitOn "\n" init)

parseFs :: String -> String
parseFs fs =
    concatMap ((\tokens ->
        "f\"" ++ (tokens !! 4) ++ "\" = f(\"" ++
        (tokens !! 0) ++ "\") `" ++
        toLower (head (tokens !! 1)) : tail (tokens !! 1) ++ "` f(\""
        ++ (tokens !! 2) ++ "\")\n") . splitOn " ") (splitOn "\n" fs)

getAnswer :: Int -> String
getAnswer x
 | x == 99 = "(f \"z0" ++ xstr ++ "\") * (2 ^ " ++ xstr ++ ")"
 | x < 10 = "(f \"z0" ++ xstr ++ "\") * (2 ^ " ++ xstr ++ ") + " ++ getAnswer (x + 1)
 | otherwise = "(f \"z" ++ xstr ++ "\") * (2 ^ " ++ xstr ++ ") + " ++ getAnswer (x + 1) where
    xstr = show x

main = do
    inp <- getContents
    let init:fs:_ = splitOn "\n\n" inp in
        putStr $ "aND x y = x * y\n" ++
                 "xOR x y = (x + y) `mod` 2\n" ++
                 "oR  x y = (x + y + x * y) `mod` 2\n" ++
                 parseInit init ++
                 parseFs fs ++
                 "f _ = 0\n" ++
                 "main = do\n" ++
                 "    print $ " ++
                 getAnswer 0 ++ "\n"