{-# Language MultiParamTypeClasses #-}
-- Here we define a world for Langton's ant which is dense, not sparse
-- Run away in abject terror from the abuse of lazy evaluation
-- Robert 'Probie' Offner
-- March 2016

module Datastructures.DenseWorld where

import Datastructures.Ant
import Datastructures.Cells
import Datastructures.Transitions
import Internals.Stream
import Internals.WorldLike
import Drawing.Cells

data InfiniteLine a = InfiniteLine (Stream a) a (Stream a)

data InfinitePlane a = InfinitePlane (Stream (InfiniteLine a))
                                     (InfiniteLine a)
                                     (Stream (InfiniteLine a))

moveBackwardInLine :: InfiniteLine a -> InfiniteLine a
moveBackwardInLine (InfiniteLine (l:<ls) c rs) =
  InfiniteLine ls l (c:<rs)

moveForwardInLine:: InfiniteLine a -> InfiniteLine a
moveForwardInLine (InfiniteLine ls c (r:<rs)) =
  InfiniteLine (c:<ls) r rs

moveDownInPlane :: InfinitePlane a -> InfinitePlane a 
moveDownInPlane (InfinitePlane us c (d:<ds)) =
  InfinitePlane (c:<us) d ds

moveUpInPlane :: InfinitePlane a -> InfinitePlane a
moveUpInPlane (InfinitePlane (u:<us) c ds) =
  InfinitePlane us u (c:<ds)

moveLeftInPlane :: InfinitePlane a -> InfinitePlane a
moveLeftInPlane (InfinitePlane us c ds) = InfinitePlane 
  (fmap moveBackwardInLine us)
  (moveBackwardInLine c)
  (fmap moveBackwardInLine ds)

moveRightInPlane :: InfinitePlane a -> InfinitePlane a
moveRightInPlane (InfinitePlane us c ds) = InfinitePlane 
  (fmap moveForwardInLine us)
  (moveForwardInLine c)
  (fmap moveForwardInLine ds)

makeLine :: a -> InfiniteLine a
makeLine x = InfiniteLine
  xline
  x
  xline
  where xline = streamRepeat x

makePlane :: a -> InfinitePlane a
makePlane x = InfinitePlane
  xplane
  (makeLine x)
  xplane
  where xplane = streamRepeat (makeLine x)

data DenseWorld = DenseWorld { antDirection :: Direction
                             , antSystem :: [Transition SquareTurn]
                             , theWorld :: InfinitePlane Cell
                             , minX :: XCoord
                             , maxX :: XCoord
                             , minY :: YCoord
                             , maxY :: YCoord
                             }

addCoordsToLine :: InfiniteLine CellState -> Integer -> InfiniteLine Cell
addCoordsToLine (InfiniteLine ls c rs) y = InfiniteLine
  (streamZip (\x cell -> Cell (Coord x y) cell) negatives ls)
  (Cell (Coord 0 y) c)
  (streamZip (\x cell -> Cell (Coord x y) cell) positives rs)

addCoords :: InfinitePlane CellState -> InfinitePlane Cell
addCoords (InfinitePlane us c ds) = InfinitePlane
  (streamZip addCoordsToLine us positives)
  (addCoordsToLine c 0) 
  (streamZip addCoordsToLine ds negatives)

-- Build a new world
newDenseWorld :: Ant -> (DenseWorld, CellState)
newDenseWorld (Ant coord dir trans) = (DenseWorld dir trans
  (addCoords (makePlane def)) 0 0 0 0, def)
  where def = CellState (repeatList (zipWith const [0..] trans))

instance WorldLike DenseWorld Ant where
  -- Yuck
  worldContents (DenseWorld _  _ (InfinitePlane us c ds) minx maxx miny maxy) =
      concat $ map lineTake (takeWhileS ((>= miny) . yCoord . point) ds)
      ++ lineTake c:map lineTake (takeWhileS ((<= maxy) . yCoord . point) us)
    where lineTake (InfiniteLine ls c rs) = takeWhileS ((>= minx) . xCoord . cellPosition) ls
            ++ c:takeWhileS ((<= maxx) . xCoord . cellPosition) rs
          point (InfiniteLine _ (Cell c _)  _ ) = c
  -- A good example of when not to pattern match and doing it anyway
  worldAnts  (DenseWorld dir trans
              (InfinitePlane _ (InfiniteLine _ (Cell c _) _ )_) _ _ _ _)
    = [Ant c dir trans] 
  worldShape _    = Square
