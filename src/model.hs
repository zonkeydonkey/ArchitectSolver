module Model where

import Utils

data TriLogic = TriTrue | TriFalse | Invalid deriving (Enum, Eq)

data FieldType = House | GasTank (Int, Int) | None | Empty deriving (Show, Eq)

type Row = [FieldType]

type DescNumbers = [Int]

type HousesList = [(Int, Int)]

type Board = [Row]

-- take care BoundedBoard type is used as synonym for rectangular two dimentional array + its size 
data BoundedBoard = BoundedBoard Board Int Int deriving (Show)

data ArchitectBoard = ArchitectBoard BoundedBoard DescNumbers DescNumbers

getBoard :: ArchitectBoard -> Board

getBoard (ArchitectBoard (BoundedBoard board rowNb columnNb) rowDscNb columnDscNb) = board


getUnboundedBoard :: BoundedBoard -> Board

getUnboundedBoard (BoundedBoard board rowNb columnNb) = board


getBoundedBoard :: ArchitectBoard -> BoundedBoard

getBoundedBoard (ArchitectBoard boundedBoard rowDscNb columnDscNb) = boundedBoard
		
		
getHouses :: Board -> HousesList

getHouses board = findIndicesTwoDim (\x -> x == House) board


getRowNb :: BoundedBoard -> Int

getRowNb (BoundedBoard board rowNb columnNb) = rowNb


getColumnNb :: BoundedBoard -> Int

getColumnNb (BoundedBoard board rowNb columnNb) = columnNb


showRow :: Row -> String

showRow [] = ""

showRow (head:tail) = (show head) ++ "\t" ++ showRow tail 


showBoard :: Board -> String

showBoard [] = ""

showBoard (headRow:tailRows) = 
	showRow headRow ++ 
	"\n"
	++ showBoard tailRows
															

printBoard :: Board -> IO()															
															
printBoard board = putStr (showBoard board)		


fieldEq :: BoundedBoard -> Int -> Int -> FieldType -> TriLogic		-- TODO - trilogic (really needed???)

fieldEq (BoundedBoard board columnNb rowNb) rowIndex columnIndex field = 
	if rowIndex > -1 && rowIndex < rowNb && columnIndex > -1 && columnIndex < columnNb then
		if board !! rowIndex !! columnIndex == field then
			TriTrue
		else
			TriFalse
	else
		Invalid
													
													
neighbourEq :: BoundedBoard -> Int -> Int -> FieldType -> Bool

neighbourEq board rowIndex columnIndex field = 
	(fieldEq board (rowIndex - 1) columnIndex field) == TriTrue ||  
	(fieldEq board rowIndex (columnIndex - 1) field) == TriTrue ||
	(fieldEq board (rowIndex + 1) columnIndex field) == TriTrue ||
	(fieldEq board rowIndex (columnIndex + 1) field) == TriTrue		


setFieldsWhen :: BoundedBoard -> (FieldType -> Bool) -> FieldType -> [(Int, Int)] -> Board

setFieldsWhen (BoundedBoard board rowNb columnNb) predicate newType indices = 
	[
		[
			if (j, i) `elem` indices && predicate (board !! j !! i) then
				newType
			else
				board !! j !! i
			| i <- [0..(columnNb - 1)]
		]
		| j <- [0..(rowNb - 1)] 
	]