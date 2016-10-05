{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- Here we define things that a similar to a world. You shouldn't need
-- to care about this unless you want to define your own data structure
-- for worlds
-- Robert 'Probie' Offner
-- Feb 2016

module Internals.WorldLike where

import Drawing.Cells
import Datastructures.Cells

class WorldLike a b | a -> b where
  worldContents :: a -> [Cell]
  worldShape    :: a -> Shape
  worldAnts     :: a -> [b]

