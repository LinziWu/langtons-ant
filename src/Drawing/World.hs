-- Here we define how to draw the world.
-- If you need to change this, you're doing something very weird
-- or even look at his
-- Robert 'Probie' Offner
-- Feb 2016

module Drawing.World where

import Internals.AntLike
import Datastructures.Cells
import Internals.WorldLike

import Drawing.Cells

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

-- Draw the cells and draw the ants
drawWorld :: (WorldLike a b, AntLike b c) => Int -> Int -> a -> Picture
drawWorld initial offset world =
  Pictures [ Pictures (map (drawCell initial offset (worldShape world))
                       $ worldContents world)
           , Pictures (map antImage $ worldAnts world)
           ]
