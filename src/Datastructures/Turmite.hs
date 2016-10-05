{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# Language MultiParamTypeClasses #-}
-- Here we define an Turmite. A Turmite is like an ant, but with its
-- own internal state
-- Robert 'Probie' Offner
-- Feb 2016

module Datastructures.Turmite where

import Internals.AntLike
import Datastructures.Transitions
import Datastructures.Cells
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

-- Unfortunately a Turmite is a bit more complex than an Ant, since it contains
-- a more complex transition which cares about its own state.

data Turmite a b = Turmite { turmitePosition :: Coord
                           , turmiteOrientation :: Direction
                           , turmiteState :: a
                           , turmiteTransition :: b}
  deriving (Show, Eq)

-- You may or may not want to use this. This is just here to clarify things
type SimpleTurmite = Turmite Integer [(Integer, Colour, Integer, Direction)]

instance AntLike SimpleTurmite (Integer, Colour, Integer, Direction) where
  antImage (Turmite (Coord x y) dir _ _ ) =
    Translate (fromIntegral x - 0.5) (fromIntegral y - 0.5) $
    Color (makeColor 0.5 0.5 0.5 1) $ case dir of
      N -> Polygon [(0,0),(1,0),(0.5,0.5)]
      S -> Polygon [(0,1),(1,1),(0.5,0.5)]
      E -> Polygon [(0,0),(0,1),(0.5,0.5)]
      W -> Polygon [(1,1),(1,0),(0.5,0.5)]
