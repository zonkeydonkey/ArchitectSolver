module Main where

import Solver
import Model
import System.IO 
import Data.Maybe 


main :: IO ()
main = do
    putStrLn "Enter filename to load board from a file"
    filename <- getLine
    contents <- readFile filename  
    let [a,b,c] = lines contents      -- split on newlines
    let left  = read a :: [Int] 
    let top = read b :: [Int]    
    let housesPositions  = read c :: [Coords]  
    let blankBoard = replicate (length left) (replicate (length top) Unchecked)
    let board = setFieldsWhen (BoundedBoard blankBoard (length left) (length top)) (\x -> x == Unchecked) House housesPositions
    putStrLn "\n++++++++++++++++++++++++++++++++++++++++++ Puzzle ++++++++++++++++++++++++++++++++++++++++++"
    prettyPrintBoard board

    let solvedBoard = solve (ArchitectBoard (BoundedBoard board (length left) (length top)) left top)
    putStrLn "\n\n+++++++++++++++++++++++++++++++++++++++++ Solution +++++++++++++++++++++++++++++++++++++++++"
    case solvedBoard of
        Nothing -> putStrLn "No result..." 
        Just x -> prettyPrintBoard x
    putStrLn "\n\n"
    putStrLn ("Enter filename to save board to a file")
    filename <- getLine
    case solvedBoard of
        Nothing -> putStrLn "No result..."
        Just x -> writeFile filename (prettyBoardToString x)
    putStrLn("File saved to "++filename) 


prettyRowToString :: Row -> String

prettyRowToString [] = ""

prettyRowToString (head:tail) = case head of 
    House ->     "House         \t" 
    None ->      "X             \t"
    Unchecked -> "NN            \t"  
    _ -> show head ++ "\t"

    ++

    prettyRowToString tail


prettyBoardToString :: Board -> String

prettyBoardToString [] = ""

prettyBoardToString (head:tail) = 
    prettyRowToString head ++
    "\n" ++
    prettyBoardToString tail


prettyPrintBoard :: Board -> IO()

prettyPrintBoard board = putStr (prettyBoardToString board)