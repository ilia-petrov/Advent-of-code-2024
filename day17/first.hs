import Data.Bits (Bits(xor))
import Data.List.Split (splitOn, splitOneOf)

doInstr :: [Int] -> [Int] -> Int -> [Int]
doInstr program combos pos
 | pos < 0 || pos >= length program = []
 | instr == 0 = doInstr program (take 4 combos ++ [combos !! 4 `div` (2 ^ (combos !! opnd))] ++ drop 5 combos) (pos + 2)
 | instr == 1 = doInstr program (take 5 combos ++ [combos !! 5 `xor` opnd] ++ drop 6 combos)                   (pos + 2)
 | instr == 2 = doInstr program (take 5 combos ++ [(combos !! opnd) `mod` 8] ++ drop 6 combos)                 (pos + 2)
 | instr == 3 = if combos !! 4 == 0 then doInstr program combos (pos + 2) else
                doInstr program combos opnd
 | instr == 4 = doInstr program (take 5 combos ++ [(combos !! 5) `xor` (combos !! 6)] ++ drop 6 combos)        (pos + 2)
 | instr == 5 = (combos !! opnd) `mod` 8 : doInstr program combos (pos + 2)
 | instr == 6 = doInstr program (take 5 combos ++ [combos !! 4 `div` (2 ^ (combos !! opnd))] ++ drop 6 combos) (pos + 2)
 | instr == 7 = doInstr program (take 6 combos ++ [combos !! 4 `div` (2 ^ (combos !! opnd))] ++ drop 7 combos) (pos + 2)
 where instr = program !! pos
       opnd  = program !! (pos + 1)

main = do
    inp <- getContents
    let lns = lines inp
        a = read (splitOn " " (lns !! 0) !! 2) :: Int
        b = read (splitOn " " (lns !! 1) !! 2) :: Int
        c = read (splitOn " " (lns !! 2) !! 2) :: Int
        program = map (\num -> read num :: Int) $ tail $ splitOneOf " ," $ lns !! 4 in
            print $ doInstr program [0,1,2,3,a,b,c] 0