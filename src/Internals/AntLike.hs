{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
-- Here we define things that are like an ant. You shouldn't need to
-- change this unless you want a new kind of ant we haven't supplied
-- But we do provide a useful helper function
-- Robert 'Probie' Offner
-- Feb 2016

module Internals.AntLike where

import Datastructures.Cells
import Datastructures.Transitions

import Graphics.Gloss.Data.Picture

class AntLike a b | a -> b where
  antImage :: a -> Picture
