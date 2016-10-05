{-# Language MultiParamTypeClasses #-}
-- Here we define an Ant. An Ant is just a position, a direction
-- and a transition system
-- Robert 'Probie' Offner
-- Feb 2016

module Datastructures.Ant where

import Internals.AntLike
import Datastructures.Cells
import Datastructures.Transitions

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

-- [Transition SquareTurn] is the list of moves the Ant can make, where
-- the head of the list is what direction to turn on Colour 0. 
-- Each subsequent element in the list is what to do when the Ant
-- walks across that state.
--
-- For Example: [L,L,R,R] has 4 Colours. When the Ant is on a Cell
-- with Colour 0 or 1 the Ant turn left. When the Ant is on a Cell
-- with Colour 2 or 3 the Ant will turn right.
--
-- Colour 0 is black by default.
data Ant = Ant { antPosition :: Coord
               , antOrientation :: Direction
               , antTransition :: [Transition SquareTurn]}
  deriving (Show, Eq)

data HexAnt = HexAnt { hexantPosition :: Coord
                     , hexantOrientation :: HexDirection
                     , hexantTransition :: [Transition HexTurn]}
  deriving (Show, Eq)

-- Don't worry too much about this. This is just needed to make it super
-- easy to add extensions. If something is an ant, we need to be able to
-- draw it

instance AntLike Ant SquareTurn where
  antImage (Ant (Coord x y) dir _) = 
    Translate (fromIntegral x - 0.5) (fromIntegral y - 0.5) $
      Color (makeColor 0.5 0.5 0.5 1) $ case dir of
        N -> Polygon [(0,0),(1,0),(0.5,0.5)]
        S -> Polygon [(0,1),(1,1),(0.5,0.5)]
        E -> Polygon [(0,0),(0,1),(0.5,0.5)]
        W -> Polygon [(1,1),(1,0),(0.5,0.5)]

instance AntLike HexAnt HexTurn where
  antImage (HexAnt (Coord x y) dir _) = 
    Translate (fromIntegral a * 6 / 8) (sqrt 3 / 4 * fromIntegral (c-b))
      $ Color (makeColor 0.5 0.5 0.5 1) $
        Rotate (60 * fromIntegral (fromEnum dir)) $ Polygon [(0,0)
                                                          ,(-0.25,-sqrt 3 / 4)
                                                          ,(0.25,-sqrt 3 /4)]
    where (a,b,c) = (x, (-x)-y, y)
