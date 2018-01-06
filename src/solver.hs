module Solver where

import Model
import Utils


board =  ArchitectBoard -- test TODO - delete
	(BoundedBoard 
	[
		[Unchecked, House, Unchecked, Unchecked, Unchecked, Unchecked],
		[Unchecked, Unchecked, Unchecked, Unchecked, Unchecked, Unchecked],
		[Unchecked, Unchecked, Unchecked, Unchecked, Unchecked, Unchecked],
		[Unchecked, Unchecked, House, Unchecked, House, Unchecked],
		[House, Unchecked, Unchecked, Unchecked, House, Unchecked],
		[Unchecked, Unchecked, House, Unchecked, House, House]
	] 6 6)
	[1, 0, 2, 1, 2, 1]
	[1, 1, 2, 1, 1, 1]
	

solve :: ArchitectBoard -> Board -- TODO - or boundedboard??? 

solve architectBoard = do
	let houses = getHouses (getBoard architectBoard)
	let newBoardHouses = setHouseHas1UncheckedNeighbour architectBoard houses
	let newHouses = deleteElems houses (snd newBoardHouses)
	getBoard  (excludeFields (fst newBoardHouses))
	
	
	





excludeFields :: ArchitectBoard -> ArchitectBoard

excludeFields (ArchitectBoard (BoundedBoard board rowNb columnNb) rowDscNb columnDscNb) = ArchitectBoard 
	(BoundedBoard
		[	
			[	
				if field == Unchecked then
					if jDscNb == 0 || iDscNb == 0 then
						None
					else
						if neighbourEq (BoundedBoard board rowNb columnNb) j i House == True then
							field
						else 
							None
				else
					field 		
				| i <- [0..(columnNb - 1)], let field = board !! j !! i, let iDscNb = columnDscNb !! i
			] 
			| j <- [0..(rowNb - 1)], let jDscNb = rowDscNb !! j
		] 
		rowNb columnNb
	) 
	rowDscNb columnDscNb
	
	
setHouseHas1UncheckedNeighbour :: ArchitectBoard -> HousesList -> (ArchitectBoard, HousesList) 

setHouseHas1UncheckedNeighbour architectBoard [] = (architectBoard, [])

setHouseHas1UncheckedNeighbour architectBoard (headHouse:tailHouses) = do
	let houseNeighbours = getUncheckedNeighbours headHouse (getBoundedBoard architectBoard)
	if length houseNeighbours == 1 then do
		let tempArchBoard = setGasTank architectBoard headHouse (head houseNeighbours)
		let tempResult = setHouseHas1UncheckedNeighbour tempArchBoard tailHouses
		(fst tempResult, [headHouse] ++ (snd tempResult))
	else
		setHouseHas1UncheckedNeighbour architectBoard tailHouses
		
		
getUncheckedNeighbours :: (Int, Int) -> BoundedBoard -> [(Int, Int)]

getUncheckedNeighbours (rowIndex, columnIndex) boundedBoard = 
	fieldEqAsList boundedBoard (rowIndex - 1, columnIndex) Unchecked 
	++
	fieldEqAsList boundedBoard (rowIndex + 1, columnIndex) Unchecked 
	++
	fieldEqAsList boundedBoard (rowIndex, columnIndex - 1) Unchecked
	++
	fieldEqAsList boundedBoard (rowIndex, columnIndex + 1) Unchecked
		
		
getCornerUncheckedNeighbours :: (Int, Int) -> BoundedBoard -> [(Int, Int)]

getCornerUncheckedNeighbours (rowIndex, columnIndex) boundedBoard = 
	fieldEqAsList boundedBoard (rowIndex - 1, columnIndex - 1) Unchecked 
	++
	fieldEqAsList boundedBoard (rowIndex - 1, columnIndex + 1) Unchecked 
	++
	fieldEqAsList boundedBoard (rowIndex + 1, columnIndex - 1) Unchecked
	++
	fieldEqAsList boundedBoard (rowIndex + 1, columnIndex + 1) Unchecked
		
		
setGasTank :: ArchitectBoard -> (Int, Int) -> (Int, Int) -> ArchitectBoard

setGasTank (ArchitectBoard boundedBoard rowDscNb cloumnDscNb) (houseRow, houseCol) (gasRow, gasCol) = 
	ArchitectBoard
		(BoundedBoard 
			(setGasTankOnBoard boundedBoard (houseRow, houseCol) (gasRow, gasCol))
			(getRowNb boundedBoard)
			(getColumnNb boundedBoard)
		)
		(decNthElem rowDscNb gasRow)
		(decNthElem cloumnDscNb gasCol)
	
	
setGasTankOnBoard :: BoundedBoard -> (Int, Int) -> (Int, Int) -> Board

setGasTankOnBoard boundedBoard (houseRow, houseCol) (gasRow, gasCol) = do
	let tempBoard = setNthMthTwoDim (getUnboundedBoard boundedBoard) (GasTank (houseRow, houseCol)) gasRow gasCol
	
	let uncheckedNeighbours = 
		(getUncheckedNeighbours (gasRow, gasCol) boundedBoard) 
		++
		(getCornerUncheckedNeighbours (gasRow, gasCol) boundedBoard)

	let newBoundedBoard = BoundedBoard tempBoard (getRowNb boundedBoard) (getColumnNb boundedBoard)
	setFieldsWhen newBoundedBoard (\x -> x == Unchecked) None uncheckedNeighbours
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
