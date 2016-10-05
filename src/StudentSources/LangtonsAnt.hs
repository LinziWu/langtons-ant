-- Here is where you'll define both Langton's Ant, and how
-- to transition a world
-- You'll definitely want to change this
-- Robert 'Probie' Offner
-- ALEXANDER JONES U5956709
-- The report is in the main directory /comp1100-2016-assignment1 and called REPORT.md
-- Feb 2016

module StudentSources.LangtonsAnt where

-- The import are here to include the content from other files.
-- You can find each file in src/Datastructures
import Datastructures.Ant
import Datastructures.Cells
import Datastructures.SquareWorld
import Datastructures.Transitions

-- #####################################################################
-- ######################## Part 0 Starts Here #########################
-- #####################################################################

-- This function is already done for you! No modification is needed.
-- We wrote this for you to give you an idea of how the
-- turnAnt and moveAnt should be used. When you run
-- ./run_langtons_ant -m part0
-- this function is called to transition the world
walkInACircle :: SquareWorld -> SquareWorld
walkInACircle squareWorld = moveAnt (turnAnt L squareWorld)

-- Students: Please implement this function.
--
-- | The 'turnAnt' function turns the ant to the given direction in the world.
--
-- The first parameter ('direction') specified the direction to which the ant
-- shall turn. The second parameter ('squareWorld') is the old world. This
-- function shall return a new 'SquareWorld' value which differs from the
-- 'squareWorld' parameter by only the 'theAnt' field. The new ant in the new
-- world shall differ from the old 'theAnt' filed by only the 'antOrientation'
-- field, which is the old value turned to the direction specified by the
-- 'direction' parameter. For example, if the old 'antOrientation' value was W
-- and the 'direction' parameter is R, the new 'antOrientation' value shall be
-- N.
--
-- Right now all this does is take the current SquareWorld and return the
-- SquareWorld unchanged. You need to take the Ant in the squareWorld and
-- change its Direction.

-- The program compiles and runs successfully on Mac OSX 64-bit. The triangle moves
-- around in a circle as required.
-- It is possible to do the turnAnt function with just Ant -> Ant but I decided to use
-- helper functions to make my code cleaner and easier for me to understand.
-- In this function, I simply hard coded all possible combinations of squareTurn and antOrientation.

updateAntDirection :: SquareTurn -> Ant -> Direction
updateAntDirection squareTurn ant
  | squareTurn == L && direction == N = W
  | squareTurn == L && direction == S = E
  | squareTurn == L && direction == E = N
  | squareTurn == L && direction == W = S

  | squareTurn == R && direction == N = E
  | squareTurn == R && direction == S = W
  | squareTurn == R && direction == E = S
  | squareTurn == R && direction == W = N
  where
    direction = antOrientation ant

updateAntOrientation :: SquareTurn -> Ant -> Ant
updateAntOrientation squareTurn ant = ant { antOrientation = updateAntDirection squareTurn ant }

-- turnAnt takes squareTurn and squareWorld as input, and uses updateAntOrientation
-- to create a new SquareWorld with antOrientation field updated within theAnt field.

turnAnt :: SquareTurn -> SquareWorld -> SquareWorld
turnAnt squareTurn squareWorld = squareWorld { theAnt = updateAntOrientation squareTurn ant }
  where
    ant = (theAnt squareWorld)

-- Students: Please implement this function.
--
-- | The 'moveAnt' function moves the ant in the given world in its current
-- direction.
--
-- The parameter ('squareWorld') is the current world. This function shall
-- return a new SquareWorld value which differs from the 'squareWorld' parameter
-- by only the 'theAnt' field. The ant in the new world shall differ from the
-- old ant by only the 'antPosition' field. The new 'antPosition' is computed by
-- moving one unit from the old 'antPosition', and the direction to move is
-- specified by the current 'antOrientation' field. For example, if the current
-- 'antOrientation' value is N and the current 'antPosition' is (Coord 0 0), the
-- new 'antPosition' value shall be (Coord 0 1).
--
-- Read the comments in the 'src/Datastructures/Cells.hs' source code for more
-- description about the coordinate system.
--
-- Just like turnAnt all it does now is return the SquareWorld without doing
-- anything. You need to get the Ant and set it's Coord to the square in front
-- of it.

-- updateCoord takes a Direction and a Coord and outputs a new
-- Coord vale with either the xCoord or yCoord field updated
-- depending on the value of direction entered.

updateCoord :: Direction -> Coord -> Coord
updateCoord direction coord
  | direction == N = coord { yCoord = yCoord coord + 1 }
  | direction == S = coord { yCoord = yCoord coord - 1 }
  | direction == E = coord { xCoord = xCoord coord + 1 }
  | direction == W = coord { xCoord = xCoord coord - 1 }

-- Since we are dealing with 'nested records', I decided to make a function that
-- uses updateCoord to update antPosition. This function takes only an Ant records
-- as input and uses the accessor functions for antOrientation and antPosition to
-- generate input for updateCoord to update antPosition field in the new Ant record.

updateAntPosition :: Ant -> Ant
updateAntPosition ant = ant { antPosition = updateCoord direction coord }
  where
    direction = antOrientation ant
    coord = antPosition ant

-- This is a simple function that takes squareWorld as input and makes a new
-- SquareWorld record with only the antPosition field updated within theAnt field.
-- The resulting SquareWorld contain an ant in which the ant is moved forawrd.

moveAnt :: SquareWorld -> SquareWorld
moveAnt squareWorld = squareWorld { theAnt = updateAntPosition ant }
  where
    ant = theAnt squareWorld

-- The following two functions simply use accessor functions of records types to extract desired fields.

-- Students: Please implement this function.
--
-- | This function extracts the 'antOrientation' field from the 'theAnt' field
-- of the parameter ('squareWorld').

getDirection :: SquareWorld -> Direction
getDirection squareWorld = antOrientation (theAnt squareWorld)

-- Students: Please implement this function.
--
-- | This function extracts the 'antPosition' field from the 'theAnt' field of
-- the parameter ('squareWorld').

getCoord :: SquareWorld -> Coord
getCoord squareWorld = antPosition (theAnt squareWorld)

-- #####################################################################
-- ######################## Part 1 Starts Here #########################
-- #####################################################################

-- Students: Please implement this function.
--
-- This is the main assessable function you need to write for Part 1.
-- When you run ./run_langtons_ant -m square this function is used.
--
-- | The 'transitionWorld' should transform the second parameter ('squareWorld')
-- as if the following actions are carried out sequentially:
--
-- 1. The Ant makes a turn according to the current cell state.
--
-- 2. Transition the cell under the Ant to its next state.
--
-- 3. The Ant moves forward by one cell.
--
-- This function is called on line 78 in src/Main.hs.
--
-- The first parameter ('defaultState') specifies the state of cells not recorded
-- in the 'theWorld' field in the second parameter ('squareWorld'). The second
-- parameter is the old state of the world. The return value shall be the new
-- state of the world, that is, a new SquareWorld value with both the 'theAnt'
-- field and the 'theWorld' field different from the old state. The Ant shall
-- have made the turn and moved forward, and the world shall contain the state
-- of the updated cell.
--
-- Hint: The rule of how the Ant should turn when standing on a cell of each
-- colour is determined by the 'antTransition' field of the Ant. See
-- 'src/Datastructure/Ant.hs' for more descriptions.
--
-- Hint: There are functions that help you find the cells at specific
-- coordinates in the square world.  See 'src/Datastructure/SquareWorld.hs'.
--
-- Hint: You should consider CellState as an opaque type. All you need to know
-- about that type is that (1) you can extract its current colour, and (2) you
-- can transition it to the next state.  See 'src/Datastructure/Cells.hs'.

-- checkCellExistence takes a SquareWorld as input and produces a boolean.
-- The point of this function is to check if the world is empty or if the ant has visited
-- the required coordinate previously. It checks if findCell returns a Nothing or a cell.
-- If a Nothing is returned, then it means the world is either empry or
-- the ant has not visited that particular coordinate previously so we have to
-- update theWorld field of squareWorld to include the new cell with the current
-- position which is extracted from theAnt.

checkCellExistence :: SquareWorld -> Bool
checkCellExistence squareWorld = case (findCell currentAntPosition squareWorld) of
  Nothing -> False
  otherwise -> True
  where
    currentAntPosition = antPosition (theAnt squareWorld)

-- getSquareTurn takes in a colour, and the list of trasitions and outputs the correct
-- SquareTurn based on the input colour.
-- I initially hard coded the colours 0 and 1 but then I looked at the Main.hs file
-- and on lines 75-79 I found the following code
-- let trans = case getArg args Trans of
--   Nothing -> [R,L]
-- I played around with this using cabal repl and realised that [Transition SquareTurn]
-- contains the "rules" for what the ant should do on a particular colour.
-- Using patterh matching it is possible to easily extract the correct SquareTurn
-- or the current colour that the ant is on.
-- This makes it possible to make the code work for transition systems other than Langton's Ant.

genSquareTurn :: Colour -> [Transition SquareTurn] -> SquareTurn
genSquareTurn colour transitions = case transitions of
  [] -> R
  x:xs
    | onColour x == colour -> turnDirection x
    | otherwise -> genSquareTurn colour xs

-- genCurrentCell takes in a SquareWorld value and extracts the current cell that
-- theAnt is on from theWorld. This function is only called in the case that theWorld
-- contains the cell that the ant is currently on so we don't need to use Maybe here.
-- It recurses through the list of cells which is extracted from theWorld field
-- until it finds a cell with the same coordinates as the ant and returns it.

genCurrentCell :: SquareWorld -> Cell
genCurrentCell squareWorld = case squareWorld of
  SquareWorld {theAnt = ant, theWorld = cells} -> case cells of
    x:xs
      | cellPosition x == currentAntPosition -> x
      | otherwise -> genCurrentCell (SquareWorld {theAnt = ant, theWorld = xs})
      where
        currentAntPosition = antPosition (theAnt squareWorld)

-- updateTheWorldTrue takes squareWorld as input and modifies the current cell.
-- The current cell is found by calling genCurrentCell function.
-- updateTheWorldTrue is called if the ant previously visited the particular coordinate.
-- updateTheWorldTrue adds a new cell to theWorld with the same position as the ant and
-- the cellState = nextCellState. It removes the current old cell using list comprehension
-- to avoid having two cells with the same coordinate in theWorld.

updateTheWorldTrue :: SquareWorld -> [Cell]
updateTheWorldTrue squareWorld = updatedCell : [cell | cell <- cells, not (cell == currentCell)]
  where
    updatedCell = Cell {cellPosition = currentAntPosition, cellState = nextCellState (cellState currentCell)}
    cells = theWorld squareWorld
    currentAntPosition = antPosition (theAnt squareWorld)
    currentCell = genCurrentCell squareWorld

-- updateTheWorldFalse takes the defaultState and squareWorld as input and add a new
-- cell to the world with position = antPosition and cellState = nextCellState.
-- This function is called when the current cell does not exist in theWorld.
-- No cells need to be removed from theWorld because the ant has not visited that
-- coordinate previously so there is no change of having duplicate cells in this case.

updateTheWorldFalse :: CellState -> SquareWorld -> [Cell]
updateTheWorldFalse defaultState squareWorld = cell : cells
  where
    cell = Cell {cellPosition = currentAntPosition, cellState = nextCellState defaultState}
    cells = theWorld squareWorld
    currentAntPosition = antPosition (theAnt squareWorld)

-- transitionWorld determines if theAnt is currently on a cell that exists in
-- theWorld or not using the checkCellExistence function.
-- True case:
-- If the cell that the ant is on exists in the world, then we replace it with a new cell
-- with the current cellState updated to the nextCellState. We remove the old cell using
-- list comprehension to avoid having two cells with the same coordinate in theWorld.
-- Similarly, this new squareWorld value and the approperiate SquareTurn value are
-- passed into the turnAnt function, which produced a squareWorld with the ant's direction changed.
-- This squareWorld value is then passed into moveAnt to produce a squareWorld value
-- with the ant moved one step forward.
-- False case:
-- If the cell that the ant is on does not exist in the world, then we add it to the list of cells
-- currently in theWorld. The new cell has the same position as theAnt and its cellState
-- is the nextCellState from the defaultState. Once we added the new cell to theWorld
-- we pass this new squareWorld value into turnAnt as well as a SquareTurn value
-- which we get from antTransition using genSquareTurn.
-- This will produce a squareWorld with the ant turned to the correct direction
-- and the new cell added to theWorld. This new squareWorld value is then passed into
-- moveAnt which moves the ant one step forward.

transitionWorld :: CellState -> SquareWorld -> SquareWorld
transitionWorld defaultState squareWorld = case (checkCellExistence squareWorld) of
  True -> moveAnt (turnAnt genSquareTurnTrue newSquareWorldTrue)
    where
      genSquareTurnTrue = genSquareTurn currentColour transitions
      newSquareWorldTrue = SquareWorld {theAnt = ant, theWorld = updateTheWorldTrue squareWorld}
      currentColour = getStateColour (cellState (genCurrentCell squareWorld))
      -- genCurrentCell is called only if we know that the cell exists in theWorld
  False -> moveAnt (turnAnt genSquareTurnFalse newSquareWorldFalse)
    where
      genSquareTurnFalse = genSquareTurn (getStateColour defaultState) transitions
      -- if the ant has not visited the cell, then we know that its colour is defaultState, i.e. black
      newSquareWorldFalse = SquareWorld {theAnt = ant, theWorld = updateTheWorldFalse defaultState squareWorld}
  where
    transitions = antTransition ant
    ant = theAnt squareWorld

-- ############### Allows for different transition systems #############

-- Students: Implement this function if you want to support different transition
-- systems.
--
-- This function is not meant to help you write transitionWorld.
-- When you run ./run_langtons_ant -m square -t LLRR
-- the "LLRR" is passed to this function. When you implement this
-- you will be able to test out different transition systems
--
-- | The 'readSquareTransition' function takes a String ('str') as parameter.
-- The string may be arbitrarily long. For valid inputs, each character in the
-- string is either L or R. It is invalid if it contains any other characters.
-- For any valid input, this function shall return (Just turns) where 'turns' is
-- a list of SquareTurn values (L or R) matching each input character (L or R).
-- Return Nothing for any invalid inputs.
--
-- Examples:
--
-- >>> readSquareTransition "LLRL"
-- Just [L,L,R,L]
--
-- >>> readSquareTransition "LLRRLRLLRL"
-- Just [L,L,R,R,L,R,L,L,R,L]
--
-- >>> readSquareTransition "LLRPLLL"
-- Nothing
--
-- Because "P" is not a valid character, the entire output is Nothing.
--
-- >>> readSquareTransition ""
-- Just []
--
-- The empty string is still valid because it does not contain any invalid
-- characters.

-- unMaybefyList is a function that removes the Maybe from a Maybe List and returns List.
-- This function is useful because it allows us to use (:) in readSquareTransition to
-- recurse through the input string and add new SquareTurn values to the output list.

unMaybefyList :: Maybe [a] -> [a]
unMaybefyList maybeList = case maybeList of
  Nothing   -> [] -- error "Illegal Inputs"
  Just list -> list

-- readSquareTransition uses pattern matching to determine which SquareTurn value
-- should be outputed for each input Char. It then recurses through the string adding
-- new SquareTurn values to the output until it reaches the empty string for which Nothing is returned.

readSquareTransition :: String -> Maybe [SquareTurn]
readSquareTransition str = case str of
  []         -> Nothing
  ('R':rest) -> Just ( R : unMaybefyList (readSquareTransition rest) )
  ('L':rest) -> Just ( L : unMaybefyList (readSquareTransition rest) )
  _          -> Nothing
