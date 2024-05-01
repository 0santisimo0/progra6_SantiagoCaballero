module BlanacedGenerator where

import System.IO
import System.Random
import Control.Monad (liftM)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.List (elemIndex)

type StudentName = String

data Student = Student{
    lastName::String,
    name::StudentName
    } deriving (Show, Eq,Ord)

data SchoolHouse = Griffindor
                    | Hufflepuff
                    | Ravenclaw
                    | Slytherin
                    deriving (Eq, Show, Ord, Read)


assignSchoolHouse :: [Int] -> IO SchoolHouse
assignSchoolHouse sizes = do
    index <- getShortestGroupIndex sizes
    return $ case index of
        0 -> Griffindor
        1 -> Hufflepuff
        2 -> Ravenclaw
        _ -> Slytherin


getShortestGroupIndex :: [Int] -> IO Int
getShortestGroupIndex sizes = do
    let indexedSizes = zip sizes [0..]
        minSize = minimum sizes
        minIndices = map snd $ filter (\(s, _) -> s == minSize) indexedSizes
    if length minIndices == 1
        then return $ head minIndices
        else pickRandom minIndices

pickRandom :: [Int] -> IO Int
pickRandom xs = do
    idx <- randomRIO (0, length xs - 1)
    return (xs !! idx)


split' :: Eq a => a -> [a] -> [[a]]
split' d [] = []
split' d s = x : split' d (drop 1 y)
             where (x,y) = span (/= d) s

parseLine :: String -> Student
parseLine line =
    let [lastName, name] = split' ',' line
    in Student{lastName=lastName, name=name}

readFileToList :: FilePath -> IO [Student]
readFileToList filePath = do
    contents <- readFile filePath
    let students = map parseLine (lines contents)
    return students

incrementAtIndex :: Int -> [Int] -> [Int]
incrementAtIndex _ [] = []
incrementAtIndex index (x:xs)
    | index == 0 = (x + 1) : xs
    | otherwise = x : incrementAtIndex (index - 1) xs


getHouseIndex :: String -> IO Int
getHouseIndex houseName =
    return $ maybe (-1) id (elemIndex houseName ["Griffindor", "Hufflepuff", "Ravenclaw", "Slytherin"])


printStudentHouse :: [Student] -> [Int] -> IO ()
printStudentHouse [] sizes = putStrLn $ show sizes
printStudentHouse (x:xs) sizes = do
    schoolHouse <- assignSchoolHouse sizes
    houseIndex <- getHouseIndex (show schoolHouse)
    putStrLn $ show (x, schoolHouse)
    putStrLn $ show sizes
    printStudentHouse xs (incrementAtIndex houseIndex sizes)


main :: IO()
main = do
    records <- readFileToList "task1_balanceStudents/textFiles/list.txt"
    printStudentHouse records [0, 0, 0, 0]