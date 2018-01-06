module Main where

import Solver
import Model

main :: IO ()
main = do
	putStrLn "Hello, Haskell!"
	temp <- getLine
	putStrLn temp