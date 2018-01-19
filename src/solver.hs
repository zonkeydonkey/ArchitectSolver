module Solver where

import Data.Maybe
import Model
import Utils
	

solve :: ArchitectBoard -> Maybe Board

solve architectBoard = do
	let houses = getHouses (getBoard architectBoard)
	let newBoardHouses = setHouseHas1UncheckedNeighbour architectBoard houses
	let newHouses = deleteElems houses (snd newBoardHouses)
	let newBoard = excludeFields (fst newBoardHouses)
	let result = setRemainingGasTanks newBoard newHouses

	if isNothing result then
		Nothing
	else do
		let resultBoard = getBoard (fromJust result)
		let rowNb = getRowNb (getBoundedBoard architectBoard)
		let columnNb = getColumnNb (getBoundedBoard architectBoard)
		Just (setNthMthTwoDimWhen (\x -> x == Unchecked) resultBoard rowNb columnNb None)


excludeFields :: ArchitectBoard -> ArchitectBoard

excludeFields (ArchitectBoard (BoundedBoard board rowNb columnNb) rowDscNb columnDscNb) = ArchitectBoard 
	(BoundedBoard
		[	
			[	
				if field == Unchecked then
					if jDscNb == 0 || iDscNb == 0 then
						None
					else
						if neighbourEq (BoundedBoard board rowNb columnNb) j i House == Prelude.True then
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
	
	
setHouseHas1UncheckedNeighbour :: ArchitectBoard -> CoordsList -> (ArchitectBoard, CoordsList) 

setHouseHas1UncheckedNeighbour architectBoard [] = (architectBoard, [])

setHouseHas1UncheckedNeighbour architectBoard (headHouse:tailHouses) = do
	let houseNeighbours = getUncheckedNeighbours headHouse (getBoundedBoard architectBoard)
	if length houseNeighbours == 1 then do
		let tempArchBoard = setGasTank architectBoard headHouse (head houseNeighbours)
		let tempResult = setHouseHas1UncheckedNeighbour tempArchBoard tailHouses
		(fst tempResult, [headHouse] ++ (snd tempResult))
	else
		setHouseHas1UncheckedNeighbour architectBoard tailHouses
		
		
getUncheckedNeighbours :: Coords -> BoundedBoard -> CoordsList

getUncheckedNeighbours (rowIndex, columnIndex) boundedBoard = 
	fieldEqAsList boundedBoard (rowIndex - 1, columnIndex) Unchecked
	++
	fieldEqAsList boundedBoard (rowIndex + 1, columnIndex) Unchecked
	++
	fieldEqAsList boundedBoard (rowIndex, columnIndex - 1) Unchecked
	++
	fieldEqAsList boundedBoard (rowIndex, columnIndex + 1) Unchecked
		
		
getCornerUncheckedNeighbours :: Coords -> BoundedBoard -> CoordsList

getCornerUncheckedNeighbours (rowIndex, columnIndex) boundedBoard = 
	fieldEqAsList boundedBoard (rowIndex - 1, columnIndex - 1) Unchecked 
	++
	fieldEqAsList boundedBoard (rowIndex - 1, columnIndex + 1) Unchecked 
	++
	fieldEqAsList boundedBoard (rowIndex + 1, columnIndex - 1) Unchecked
	++
	fieldEqAsList boundedBoard (rowIndex + 1, columnIndex + 1) Unchecked
		
		
setGasTank :: ArchitectBoard -> Coords -> Coords -> ArchitectBoard

setGasTank (ArchitectBoard boundedBoard rowDscNb cloumnDscNb) (houseRow, houseCol) (gasRow, gasCol) = 
	ArchitectBoard
		(BoundedBoard 
			(setGasTankOnBoard boundedBoard (houseRow, houseCol) (gasRow, gasCol))
			(getRowNb boundedBoard)
			(getColumnNb boundedBoard)
		)
		(decNthElem rowDscNb gasRow)
		(decNthElem cloumnDscNb gasCol)
	
	
setGasTankOnBoard :: BoundedBoard -> Coords -> Coords -> Board

setGasTankOnBoard boundedBoard (houseRow, houseCol) (gasRow, gasCol) = do
	let tempBoard = setNthMthTwoDim (getUnboundedBoard boundedBoard) (GasTank (houseRow, houseCol)) gasRow gasCol
	
	let uncheckedNeighbours = 
		(getUncheckedNeighbours (gasRow, gasCol) boundedBoard) 
		++
		(getCornerUncheckedNeighbours (gasRow, gasCol) boundedBoard)

	let newBoundedBoard = BoundedBoard tempBoard (getRowNb boundedBoard) (getColumnNb boundedBoard)
	setFieldsWhen newBoundedBoard (\x -> x == Unchecked) None uncheckedNeighbours


setRemainingGasTanks :: ArchitectBoard -> CoordsList -> Maybe ArchitectBoard

setRemainingGasTanks board [] = Just board

setRemainingGasTanks architectBoard (headHouse:tailHouses) = do
	let neighbours = getUncheckedNeighbours headHouse (getBoundedBoard architectBoard)
	setGasForNeighbours architectBoard neighbours (headHouse:tailHouses)


setGasForNeighbours :: ArchitectBoard -> CoordsList -> CoordsList -> Maybe ArchitectBoard

setGasForNeighbours board [] houses = Nothing

setGasForNeighbours board (headNeighb:tailNeighbs) (headHouse:tailHouses) = do
	let newBoard = setGasTank board headHouse headNeighb
	if isBoardOK newBoard then do
		let newNewBoard = setRemainingGasTanks newBoard tailHouses
		if isNothing newNewBoard then
			setGasForNeighbours board tailNeighbs (headHouse:tailHouses)
		else
			newNewBoard
	else
		setGasForNeighbours board tailNeighbs (headHouse:tailHouses)


isBoardOK :: ArchitectBoard -> Bool

isBoardOK (ArchitectBoard board rowDscNb columnDscNb) = 
	all (\x -> x > -1) rowDscNb
	&&
	all (\x -> x > -1) columnDscNb 
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
