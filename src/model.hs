module Model where

import Utils

data TriLogic = True | False | Invalid deriving (Enum, Eq)

type Coords = (Int, Int)

type CoordsList = [Coords]

data FieldType = House | GasTank Coords | None | Unchecked deriving (Show, Eq)

type Row = [FieldType]

type DescNumbers = [Int]

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
		
		
getHouses :: Board -> CoordsList

getHouses board = findIndicesTwoDim (\x -> x == House) board


getRowNb :: BoundedBoard -> Int

getRowNb (BoundedBoard board rowNb columnNb) = rowNb


getColumnNb :: BoundedBoard -> Int

getColumnNb (BoundedBoard board rowNb columnNb) = columnNb


rowToString :: Row -> String

rowToString [] = ""

rowToString (head:tail) = (show head) ++ "\t" ++ rowToString tail 


boardToString :: Board -> String

boardToString [] = ""

boardToString (headRow:tailRows) = 
	rowToString headRow ++ 
	"\n" ++
	boardToString tailRows
															

printBoard :: Board -> IO()															
															
printBoard board = putStr (boardToString board)		





fieldEq :: BoundedBoard -> Coords -> FieldType -> TriLogic

fieldEq (BoundedBoard board rowNb columnNb) (rowIndex, columnIndex) field = 
	if rowIndex > -1 && rowIndex < rowNb && columnIndex > -1 && columnIndex < columnNb then
		if board !! rowIndex !! columnIndex == field then
			Model.True
		else
			Model.False
	else
		Invalid


fieldEqAsList :: BoundedBoard -> Coords -> FieldType -> CoordsList

fieldEqAsList boundedBoard (rowIndex, columnIndex) field = 
	if fieldEq boundedBoard (rowIndex, columnIndex) field == Model.True then
		[(rowIndex, columnIndex)]
	else 
		[]
													
													
neighbourEq :: BoundedBoard -> Int -> Int -> FieldType -> Bool

neighbourEq board rowIndex columnIndex field = 
	(fieldEq board ((rowIndex - 1), columnIndex) field) == Model.True ||  
	(fieldEq board (rowIndex, (columnIndex - 1)) field) == Model.True ||
	(fieldEq board ((rowIndex + 1), columnIndex) field) == Model.True ||
	(fieldEq board (rowIndex, (columnIndex + 1)) field) == Model.True		


setFieldsWhen :: BoundedBoard -> (FieldType -> Bool) -> FieldType -> CoordsList -> Board

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