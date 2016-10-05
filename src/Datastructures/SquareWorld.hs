{-# Language MultiParamTypeClasses #-}
-- Here we define a simple world for Langton's ant
-- You'll want to look at this, but don't change it
-- Robert 'Probie' Offner
-- Feb 2016

module Datastructures.SquareWorld where

import Datastructures.Ant
import Datastructures.Cells
import Internals.Stream
import Internals.WorldLike

import Drawing.Cells

-- A world has a single ant and a list of cells. The world does not record the
-- state of all cells. For all cells not recorded in the 'theWorld' field, the
-- state is provided separately. For example, in the 'transitionWorld' function
-- in 'src/StudentSources/LangtonsAnt.hs', the default state for cells not
-- recorded in 'theWorld' is provided as the parameter ('firstCell')
data SquareWorld = SquareWorld { theAnt :: Ant, theWorld :: [Cell]}
  deriving Show

-- Build a new world.
newSquareWorld :: Ant -> (SquareWorld, CellState)
newSquareWorld ant =
  (SquareWorld ant [],
    CellState (repeatList (zipWith const [0..] (antTransition ant))))

-- Return a cell at a given coord
findCell :: Coord -> SquareWorld -> Maybe Cell
findCell coord (SquareWorld _ cs) = go cs
  where
    go [] = Nothing
    go (x:xs)
        | cellPosition x == coord = Just x
        | otherwise = go xs

-- Don't worry too much about the type of this function or how it is defined
-- It might be useful when debugging things though
-- If you call this on a world from within GHCi
-- it'll show you something more human readable
-- but don't call this on very large worlds, it won't work well
-- It also doesn't show the ant
showSquareWorld :: SquareWorld -> Coord -> Coord -> IO ()
showSquareWorld world (Coord minX minY) (Coord maxX maxY) =
  putStrLn prettyWorld
  where prettyWorld = unlines [concat [find_in_world x y | x <- [minX .. maxX]]
                              | y <- [minY .. maxY]]
        -- Since we can't enforce the same coordinate not being in the world twice
        -- we grab the first coord we find.
        find_in_world x y = case filter (\(Cell coord _) -> x == xCoord coord
                                                         && y == yCoord coord)
                                 (theWorld world) of
          a:_ -> show (getStateColour (cellState a))
          _ -> "."

-- And now for the stuff needed to make this work, you can ignore this
instance WorldLike SquareWorld Ant where
  worldContents   = theWorld
  worldAnts world = [theAnt world]
  worldShape _    = Square
