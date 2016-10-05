-- Here we define the cells, which in our case we represent with colours.
-- You don't really need to care about anything after getStateColour
-- But if you're feeling brave, feel free to read on
-- Robert 'Probie' Offner
-- Feb 2016

module Datastructures.Cells where

import Internals.Stream

-- A coordinate.
--
-- In our coordinate system, north is the top of the screen; south is the bottom
-- of the screen; east is the right of the screen; and west is the left of the
-- screen.  East has higher X coordinate than west; and north has higher Y
-- coordinate than south. The coordinate (Coord 0 0) is the centre of the
-- window.

type XCoord = Integer
type YCoord = Integer

data Coord = Coord {xCoord :: XCoord, yCoord :: YCoord}
  deriving (Show, Eq)

data Cell = Cell {cellPosition :: Coord, cellState :: CellState}
  deriving (Show, Eq)

type Colour = Integer

-- A function to get the next cell state. Don't worry too much about
-- how it works. 
nextCellState :: CellState -> CellState
nextCellState (CellState (_ :< xs)) = CellState xs

-- A function to get the colour of a cell state. Once again don't worry
-- too much about how it works.
getStateColour :: CellState -> Colour
getStateColour (CellState (x :< _)) = x

-- A cell state is simply a cycle of the possible colours. Don't worry too much
-- about this. As far as you're concerned CellState is an opaque type, with
-- a function to get the next colour and the current colour.
-- The newtype keyword is just a special case of the data keyword, for
-- types with exactly one constructor and one field.
-- There is a very minor difference between data and newtype
-- in that pattern matching always succeeds on a newtype
-- so for example
-- foo (SomeNewType _) = True
-- when called like
-- foo (error "What is going on?")
-- will give True, but for something declared with data
-- will throw the error "What is going on?"
newtype CellState = CellState (Stream Colour)

-- Instead of showing the cycle of colours (which is infinite), showing the
-- current position in the cycle should suffice
instance Show CellState where
  show (CellState (x :< _)) = show x

-- By construction, two cell states are equal if the currently shown
-- states are the same. Interestingly enough, if these were constructed
-- properly (i.e by the same repeat_list function where each element was
-- distinct), if they're equal they actually exist in the same place in memory,
-- thanks to a think Haskell can do called "sharing"; a nice feature
-- allowed by immutability and laziness (Unfortunately Haskell is not
-- quite smart enough to do this for any two arbitrary things that are
-- equal. Look at the definition for repeat_list to work out how it can
-- tell when two things are the same)

instance Eq CellState where
  (CellState (x :< _)) == (CellState (y :< _)) = x == y
