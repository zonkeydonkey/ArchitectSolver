module ArchitectSolver where

import Model
import Utils

solve :: ArchitectBoard -> Board -- TODO - or boundedboard???

board =  ArchitectBoard -- test TODO - delete
	(BoundedBoard 
	[
		[Empty, House, Empty, Empty, Empty, Empty],
		[Empty, Empty, Empty, Empty, Empty, Empty],
		[Empty, Empty, Empty, Empty, Empty, Empty],
		[Empty, Empty, House, Empty, House, Empty],
		[House, Empty, Empty, Empty, House, Empty],
		[Empty, Empty, House, Empty, House, House]
	] 6 6)
	[1, 0, 2, 1, 2, 1]
	[1, 1, 2, 1, 1, 1] 

solve architectBoard = do
	let houses = getHouses (getBoard architectBoard)
	let tempArchitectBoard = setHouseHas1EmptyNeighbour architectBoard houses
	getBoard {--(excludeFields--} tempArchitectBoard -- TODO
	
	
	





excludeFields :: ArchitectBoard -> ArchitectBoard

excludeFields (ArchitectBoard (BoundedBoard board rowNb columnNb) rowDscNb cloumnDscNb) = ArchitectBoard 
	(BoundedBoard
		[	
			[
				if field == House || neighbourEq (BoundedBoard board rowNb columnNb) j i House == True then  -- TODO - zrownoleglic co sie da!!!!
					field
				else
					None
				| i <- [0..(columnNb - 1)], let field = board !! j !! i
			] 
			| j <- [0..(rowNb - 1)]
		] 
		rowNb columnNb
	) 
	rowDscNb cloumnDscNb
	
	
setHouseHas1EmptyNeighbour :: ArchitectBoard -> HousesList -> ArchitectBoard

setHouseHas1EmptyNeighbour architectBoard [] = architectBoard

setHouseHas1EmptyNeighbour architectBoard (headHouse:tailHouses) = do
	let houseNeighbours = getEmptyNeighbours headHouse (getBoundedBoard architectBoard)
	if length houseNeighbours == 1 then do
		let newArchitectBoard = setGasTank architectBoard headHouse (head houseNeighbours)  
		setHouseHas1EmptyNeighbour newArchitectBoard tailHouses
	else
		setHouseHas1EmptyNeighbour architectBoard tailHouses
		
		
getEmptyNeighbours :: (Int, Int) -> BoundedBoard -> [(Int, Int)]

getEmptyNeighbours (rowIndex, columnIndex) boundedBoard = 		-- TODO - refactor??
	if fieldEq boundedBoard (rowIndex - 1) columnIndex Empty == TriTrue then
		[((rowIndex - 1), columnIndex)]
	else 
		[]
		
	++
		
	if fieldEq boundedBoard rowIndex (columnIndex - 1) Empty == TriTrue then
		[(rowIndex, (columnIndex - 1))]
	else 
		[]
		
	++
		
	if fieldEq boundedBoard (rowIndex + 1) columnIndex Empty == TriTrue then
		[((rowIndex + 1), columnIndex)]
	else 
		[]
		
	++
	
	if fieldEq boundedBoard rowIndex (columnIndex + 1) Empty == TriTrue then
		[(rowIndex, (columnIndex + 1))]
	else 
		[]
		
		
getCornerEmptyNeighbours :: (Int, Int) -> BoundedBoard -> [(Int, Int)]

getCornerEmptyNeighbours (rowIndex, columnIndex) boundedBoard = 		-- TODO - refactor??
	if fieldEq boundedBoard (rowIndex - 1) (columnIndex - 1) Empty == TriTrue then
		[((rowIndex - 1), (columnIndex - 1))]
	else 
		[]
		
	++
		
	if fieldEq boundedBoard (rowIndex - 1) (columnIndex + 1) Empty == TriTrue then
		[((rowIndex - 1), (columnIndex + 1))]
	else 
		[]
		
	++
		
	if fieldEq boundedBoard (rowIndex + 1) (columnIndex - 1) Empty == TriTrue then
		[((rowIndex + 1), (columnIndex - 1))]
	else 
		[]
		
	++
	
	if fieldEq boundedBoard (rowIndex + 1) (columnIndex + 1) Empty == TriTrue then
		[((rowIndex + 1), (columnIndex + 1))]
	else 
		[]
		
		
setGasTank :: ArchitectBoard -> (Int, Int) -> (Int, Int) -> ArchitectBoard

setGasTank (ArchitectBoard boundedBoard rowDscNb cloumnDscNb) (houseRow, houseCol) (gasRow, gasCol) = 
	(ArchitectBoard
		(BoundedBoard 
			(setGasTankOnBoard boundedBoard (houseRow, houseCol) (gasRow, gasCol))
			(getRowNb boundedBoard)
			(getColumnNb boundedBoard)
		)
		(decNthElem rowDscNb gasRow)
		(decNthElem cloumnDscNb gasCol)
	)
	
	
setGasTankOnBoard :: BoundedBoard -> (Int, Int) -> (Int, Int) -> Board -- TODO - or boundedBoard???

setGasTankOnBoard boundedBoard (houseRow, houseCol) (gasRow, gasCol) = do
	let tempBoard = setNthMthTwoDim (getUnboundedBoard boundedBoard) (GasTank (houseRow, houseCol)) gasRow gasCol
	
	let emptyNeighbours = 
		(getEmptyNeighbours (gasRow, gasCol) boundedBoard) 
		++
		(getCornerEmptyNeighbours (gasRow, gasCol) boundedBoard)
	
	setFieldsWhen boundedBoard (\x -> x == Empty) None emptyNeighbours
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
