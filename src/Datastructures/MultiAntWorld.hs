{-# Language MultiParamTypeClasses #-}
-- Here we define a world for Langton's ant which supports many ants
-- You'll want to look at this, but don't change it
-- Robert 'Probie' Offner
-- March 2016

module Datastructures.MultiAntWorld where

import Datastructures.Ant
import Datastructures.Cells
import Internals.Stream
import Internals.WorldLike
import Drawing.Cells

data MultiAntWorld = MultiAntWorld { theAnts :: [Ant], theWorld :: [Cell]
                                   , defaultCell :: CellState}
  deriving Show

-- Build a new world
newMultiAntWorld :: [Ant] -> MultiAntWorld
newMultiAntWorld ants = case ants of
  [] -> error "You need at least one ant!"
  _ -> MultiAntWorld ants []
         (CellState (repeatList (zipWith const [0..]
                                 (antTransition (head ants)))))

instance WorldLike MultiAntWorld Ant where
  worldContents = theWorld
  worldAnts     = theAnts 
  worldShape _  = Square
