import Data.Maybe
import GHC.IO.Handle (hSetBuffering, BufferMode (NoBuffering))
import System.IO (stdin)

zipLsts :: [Maybe Int] -> [Int] -> [Int]
zipLsts [] nums = nums
zipLsts (Nothing:lst1) (x:lst2) = x : zipLsts lst1 lst2
zipLsts (Just x : lst1) lst2 = x : zipLsts lst1 lst2

genFileSys :: String -> Int -> [Maybe Int]
genFileSys "" _ = []
genFileSys (ch:str) num
 | even num  = replicate (read [ch] :: Int) (Just $ num `div` 2) ++ genFileSys str (num + 1)
 | otherwise = replicate (read [ch] :: Int) Nothing ++ genFileSys str (num + 1)

genRev :: [Maybe Int] -> [Int]
genRev = foldl (\res id -> if isNothing id then res else fromJust id : res) []

checkSum :: [Int] -> Int -> Int
checkSum [] _ = 0
checkSum (x:xs) pos = x * pos + checkSum xs (pos + 1)

solve :: String -> Int
solve inp = checkSum (take (length files) $ zipLsts fileSys files) 0 where
    fileSys = genFileSys inp 0
    files = genRev fileSys

main = do
    hSetBuffering stdin NoBuffering
    inp <- getLine
    print $ solve inp