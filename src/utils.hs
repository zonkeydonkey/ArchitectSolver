module Utils where

import Data.List

findIndicesTwoDim :: (a -> Bool) -> [[a]] -> [(Int, Int)]

findIndicesTwoDim predicate twoDimList = findIndicesTwoDimFrom predicate twoDimList 0


findIndicesTwoDimFrom :: (a -> Bool) -> [[a]] -> Int -> [(Int, Int)]

findIndicesTwoDimFrom predicate [] from = []

findIndicesTwoDimFrom predicate (head:tail) from = 
	zip [from, from..] (findIndices predicate head) 
	++
	findIndicesTwoDimFrom predicate tail (from + 1)
	
	
decNthElem :: [Int] -> Int -> [Int]

decNthElem [] index = []

decNthElem (head:tail) index = 
	if index == 0 then
		(head - 1) : tail
	else 
		head : decNthElem tail (index - 1)
		
		
setNth :: [a] -> a -> Int -> [a]

setNth [] newElem index = []

setNth (head:tail) newElem index = 
	if index == 0 then
		newElem : tail
	else
		head : setNth tail newElem (index - 1)

		
setNthMthTwoDim :: [[a]] -> a -> Int -> Int -> [[a]]

setNthMthTwoDim twoDimList newElem row column = 
	take row twoDimList 
		++ 
		setNth (twoDimList !! row) newElem column 
			: 
		drop (row + 1) twoDimList


-- use when second argument is much longer than first
deleteElems ::  Eq a => [a] -> [a] -> [a]

deletedElems [] deleted = []

deleteElems (head:tail) deleted =
	if head `elem` deleted then
		deleteElems tail deleted
	else 
		head : deleteElems tail deleted
		
	




