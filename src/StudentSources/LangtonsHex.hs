-- Here is where you'll define both Langton's Ant, and how
-- to transition a world
-- You'll definitely want to change this
-- Robert 'Probie' Offner
-- ALEXANDER JONES U5956709
-- The report is in the main directory /comp1100-2016-assignment1 and called REPORT.md
-- Feb 2016

module StudentSources.LangtonsHex where

import Datastructures.Ant
import Datastructures.Cells
import Datastructures.Transitions
import Datastructures.HexWorld

-- #####################################################################
-- ######################## Part 2 Starts Here #########################
-- #####################################################################

-- Right now all this does is take the current hexWorld and
-- return the unchanged state. You need to take the Ant in the
-- hexWorld and change it's Direction.
-- So if the Ant was pointing Up and turns 60 degrees to the right (R1) it should
-- be RU

-- updateAntDirection is a function that takes a hexTurn and a hexAnt, extracts the hexantOrientation
-- from the hexAnt and produces the approperiate HexDirection based on a combination of
-- hexTurn and hexantOrientation.
-- It is possible to do the turnAnt function with just HexAnt -> HexAnt but I decided to use
-- helper functions to make my code cleaner and easier for me to understand.
-- In this function, I simply hard coded all possible combinations of hexTurn and hexantOrientation.
-- I determined the approperiate resulting HexDirection by drawing diagrams on paper and then
-- choosing a hexDirection and a hexTurn and finding a pattern for the turning.
-- For example, for L2, every hexDirection is moved back by two steps.

updateAntDirection :: HexTurn -> HexAnt -> HexDirection
updateAntDirection hexTurn hexAnt
  | hexTurn == L2 && hexDirection == Up = LD
  | hexTurn == L2 && hexDirection == RU = LU
  | hexTurn == L2 && hexDirection == RD = Up
  | hexTurn == L2 && hexDirection == Dn = RU
  | hexTurn == L2 && hexDirection == LD = RD
  | hexTurn == L2 && hexDirection == LU = Dn

  | hexTurn == L1 && hexDirection == Up = LU
  | hexTurn == L1 && hexDirection == RU = Up
  | hexTurn == L1 && hexDirection == RD = RU
  | hexTurn == L1 && hexDirection == Dn = RD
  | hexTurn == L1 && hexDirection == LD = Dn
  | hexTurn == L1 && hexDirection == LU = LD

  | hexTurn == None = hexDirection

  | hexTurn == R1 && hexDirection == Up = RU
  | hexTurn == R1 && hexDirection == RU = RD
  | hexTurn == R1 && hexDirection == RD = Dn
  | hexTurn == R1 && hexDirection == Dn = LD
  | hexTurn == R1 && hexDirection == LD = LU
  | hexTurn == R1 && hexDirection == LU = Up

  | hexTurn == R2 && hexDirection == Up = RD
  | hexTurn == R2 && hexDirection == RU = Dn
  | hexTurn == R2 && hexDirection == RD = LD
  | hexTurn == R2 && hexDirection == Dn = LU
  | hexTurn == R2 && hexDirection == LD = Up
  | hexTurn == R2 && hexDirection == LU = RU

  | hexTurn == U && hexDirection == Up = Dn
  | hexTurn == U && hexDirection == RU = LD
  | hexTurn == U && hexDirection == RD = LU
  | hexTurn == U && hexDirection == Dn = Up
  | hexTurn == U && hexDirection == LD = RU
  | hexTurn == U && hexDirection == LU = RD
  where
    hexDirection = hexantOrientation hexAnt

updateAntOrientation :: HexTurn -> HexAnt -> HexAnt
updateAntOrientation hexTurn hexAnt = hexAnt { hexantOrientation = updateAntDirection hexTurn hexAnt }

turnAnt :: HexTurn -> HexWorld -> HexWorld
turnAnt hexTurn hexWorld = hexWorld { theAnt = updateAntOrientation hexTurn hexAnt }
  where
    hexAnt = (theAnt hexWorld)

-- Just like turnAnt all it does now is return the hexWorld
-- without doing anything. You need to get the Ant and set its
-- Coord to the hex in front of it.

-- updateCoord takes a Direction and a Coord and outputs a new
-- Coord vale with either or both the xCoord or yCoord field updated
-- depending on the value of direction entered.

-- The way I did updateCoord is by drawing 7 hex cells on paper with (0,0) in the middle
-- and then finding a pattern for how the hexDirection changes the (0,0) coordinate.

updateCoord :: HexDirection -> Coord -> Coord
updateCoord hexDirection coord
  | hexDirection == Up = coord { yCoord = yCoord coord +1 }
  | hexDirection == RU = coord { xCoord = xCoord coord +1 }
  | hexDirection == RD = coord { xCoord = xCoord coord +1, yCoord = yCoord coord -1 }
  | hexDirection == Dn = coord { yCoord = yCoord coord -1 }
  | hexDirection == LD = coord { xCoord = xCoord coord -1 }
  | hexDirection == LU = coord { xCoord = xCoord coord -1, yCoord = yCoord coord +1 }

updateAntPosition :: HexAnt -> HexAnt
updateAntPosition hexAnt = hexAnt { hexantPosition = updateCoord hexDirection coord }
  where
    hexDirection = hexantOrientation hexAnt
    coord = hexantPosition hexAnt

moveAnt :: HexWorld -> HexWorld
moveAnt hexWorld = hexWorld { theAnt = updateAntPosition hexAnt }
  where
    hexAnt = theAnt hexWorld

-- This is here to help you to write your turnAnt and moveAnt.
-- All it should do is extract the Direction from the hexWorld
getDirection :: HexWorld -> HexDirection
getDirection hexWorld = hexantOrientation (theAnt hexWorld)

-- This is here to help you to write your moveAnt.
-- All it should do is extract the Coord from the hexWorld
getCoord :: HexWorld -> Coord
getCoord hexWorld = hexantPosition (theAnt hexWorld)

-- The main function you need to write. To start with it might be easier to
-- hardcode the behaviour for langton's ant, but if you want to maximize your
-- marks it should work for any ant transition system

-- I looked through the filed and could not find an equivalent to findCell
-- so I decided to make findHexCell which is the same as findCell but takes
-- HexWorld as input instead of SquareWorld.

findHexCell :: Coord -> HexWorld -> Maybe Cell
findHexCell coord (HexWorld _ cs) = go cs
  where
    go [] = Nothing
    go (x:xs)
        | cellPosition x == coord = Just x
        | otherwise = go xs

-- The remaining helper functions for transitionWorld of part 2 are the same as
-- transitionWorld for part 1 so refer to part 1 comments for explanations.

checkCellExistence :: HexWorld -> Bool
checkCellExistence hexWorld = case (findHexCell currentAntPosition hexWorld) of
  Nothing -> False
  otherwise -> True
  where
    currentAntPosition = hexantPosition (theAnt hexWorld)

genHexTurn :: Colour -> [Transition HexTurn] -> HexTurn
genHexTurn colour transitions = case transitions of
  [] -> None
  x:xs
    | onColour x == colour -> turnDirection x
    | otherwise -> genHexTurn colour xs

genCurrentCell :: HexWorld -> Cell
genCurrentCell hexWorld = case hexWorld of
  HexWorld {theAnt = hexAnt, theWorld = cells} -> case cells of
    x:xs
      | cellPosition x == currentAntPosition -> x
      | otherwise -> genCurrentCell (HexWorld {theAnt = hexAnt, theWorld = xs})
      where
        currentAntPosition = hexantPosition (theAnt hexWorld)

updateTheWorldTrue :: HexWorld -> [Cell]
updateTheWorldTrue hexWorld = updatedCell : [cell | cell <- cells, not (cell == currentCell)]
  where
    updatedCell = Cell {cellPosition = currentAntPosition, cellState = nextCellState (cellState currentCell)}
    cells = theWorld hexWorld
    currentAntPosition = hexantPosition (theAnt hexWorld)
    currentCell = genCurrentCell hexWorld

updateTheWorldFalse :: CellState -> HexWorld -> [Cell]
updateTheWorldFalse defaultState hexWorld = cell : cells
  where
    cell = Cell {cellPosition = currentAntPosition, cellState = nextCellState defaultState}
    cells = theWorld hexWorld
    currentAntPosition = hexantPosition (theAnt hexWorld)

transitionWorld :: CellState -> HexWorld -> HexWorld
transitionWorld defaultState hexWorld = case (checkCellExistence hexWorld) of
  True -> moveAnt (turnAnt genHexTurnTrue newHexWorldTrue)
    where
      genHexTurnTrue = genHexTurn currentColour transitions
      newHexWorldTrue = HexWorld {theAnt = hexAnt, theWorld = updateTheWorldTrue hexWorld}
      currentColour = getStateColour (cellState (genCurrentCell hexWorld))
  False -> moveAnt (turnAnt genHexTurnFalse newHexWorldFalse)
    where
      genHexTurnFalse = genHexTurn (getStateColour defaultState) transitions
      newHexWorldFalse = HexWorld {theAnt = hexAnt, theWorld = updateTheWorldFalse defaultState hexWorld}
  where
    transitions = hexantTransition hexAnt
    hexAnt = theAnt hexWorld

-- ############### Allows for different transition systems #############

-- Take a string of the form L1R1UN and return Just [L1,R1,U,None]

unMaybefyList :: Maybe [a] -> [a]
unMaybefyList maybeList = case maybeList of
  Nothing   -> []
  Just list -> list

-- I had a lot of trouble coming up with readHexTransition because HexTurn have
-- variable length.
-- Initially I tried to do it using x:xs and x:y:ys but then I realised that it is
-- easier using pattern matching on the first element or two of the input string
-- and then doing the same for the rest of the string using recursion.

readHexTransition :: String -> Maybe [HexTurn]
readHexTransition stringHexTurns = case stringHexTurns of
  []             -> Nothing
  ('L':'2':rest) -> Just ( L2   : unMaybefyList (readHexTransition rest) )
  ('L':'1':rest) -> Just ( L1   : unMaybefyList (readHexTransition rest) )
  ('N':rest)     -> Just ( None : unMaybefyList (readHexTransition rest) )
  ('R':'1':rest) -> Just ( R1   : unMaybefyList (readHexTransition rest) )
  ('R':'2':rest) -> Just ( R2   : unMaybefyList (readHexTransition rest) )
  ('U':rest)     -> Just ( U    : unMaybefyList (readHexTransition rest) )
  _              -> Nothing
