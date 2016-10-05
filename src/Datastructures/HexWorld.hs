{-# Language MultiParamTypeClasses #-}
-- Here we define a simple world for Langton's ant
-- You'll want to look at this, but don't change it
-- Robert 'Probie' Offner
-- Feb 2016

module Datastructures.HexWorld where

import Datastructures.Ant
import Datastructures.Cells
import Internals.Stream
import Internals.WorldLike

import Drawing.Cells

data HexWorld = HexWorld { theAnt :: HexAnt, theWorld :: [Cell]}
  deriving Show

newHexWorld :: HexAnt -> (HexWorld, CellState)
newHexWorld ant = (HexWorld ant [], CellState (repeatList (zipWith const [0..] (hexantTransition ant))))

-- And now for the stuff needed to make this work, you can ignore this
instance WorldLike HexWorld HexAnt where
  worldContents   = theWorld
  worldAnts world = [theAnt world]
  worldShape _    = Hexagon
