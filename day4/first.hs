import Data.List.Split (splitOn)
import GHC.IO.Handle (NewlineMode(inputNL))
-- line word
foundWord :: String -> String -> Int
foundWord _ "" = 1
foundWord "" _ = 0
foundWord (x:line) (y:word) = if x == y then foundWord line word else 0

totalFinds :: String -> String -> Int
totalFinds "" _ = 0;
totalFinds line word = foundWord line word + totalFinds (tail line) word

solveRows :: [String] -> String -> Int
solveRows lns word = foldr (\line total -> totalFinds line word + totalFinds (reverse line) word + total) 0 lns

transpone :: [[a]] -> [[a]]
transpone ([]:_) = []
transpone matrix = map head matrix : transpone (map tail matrix)

foundDiag :: [String] -> String -> Int
foundDiag _ "" = 1
foundDiag [] _ = 0
foundDiag ([]:_) _ = 0
foundDiag ((x:line):lns) (y:word) = if x == y then foundDiag (map tail lns) word else 0

allDiagInRow :: [String] -> String -> Int
allDiagInRow [] _ = 0
allDiagInRow ([]:_) _ = 0
allDiagInRow matr word = foundDiag matr word + allDiagInRow (map tail matr) word

allInDiag :: [String] -> String -> Int
allInDiag [] _ = 0
allInDiag matr word = allDiagInRow matr word + allInDiag (tail matr) word

inAllDiag :: [String] -> String -> Int
inAllDiag matr word =
    allInDiag matr word +
    allInDiag (map reverse matr) word +
    allInDiag (reverse matr) word +
    allInDiag (reverse $ map reverse matr) word

main = do
    inp <- getContents
    let lns = lines inp in
        print $ solveRows lns "XMAS" + solveRows (transpone lns) "XMAS" + inAllDiag lns "XMAS"