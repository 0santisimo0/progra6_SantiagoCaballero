module RandomNamePick where

import System.IO
import System.Random
import Control.Monad (liftM)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text


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


assignSchoolHouse :: IO SchoolHouse
assignSchoolHouse  = do
     house <- pickRandom [Griffindor,
      Hufflepuff, Ravenclaw, Slytherin]
     return house


pickRandom :: [a] -> IO a
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

main :: IO()
main = do
    records <- readFileToList "textFiles/list.txt"
    let studentsList [] =  return ()
        studentsList (x:xs) = do 
            schoolHouse <- assignSchoolHouse
            putStrLn $ show (x,schoolHouse)
            (studentsList xs)
    (studentsList records)