-- Here we define how to draw the colours.
-- Unless you need more than 8 states, you probably don't to modify this
-- or even look at his
-- Robert 'Probie' Offner
-- Feb 2016

module Drawing.Cells where

import Datastructures.Cells

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

data Shape = Square | Hexagon

-- A cell is simply a 1x1 rectangle of the correct colour
drawCell :: Int -> Int -> Shape -> Cell -> Picture
drawCell initial change shape (Cell (Coord x y) colour)
  | getStateColour colour == 0 = Blank
  | otherwise = Translate x' y' 
    (Color cell_colour (drawShape shape))
  where cell_colour = makeColor r g b 1
        (r,g,b) = niceColour (fromIntegral initial)
          (fromIntegral change) (getStateColour colour)
        drawShape shape = case shape of
          Square -> rectangleSolid 0.8 0.8
          Hexagon -> Scale 0.8 0.8 $ Polygon (map (`rotate` (0.5,0)) [0..5])
            where rotate n (x,y) = let θ = n * (pi / 3) in
                    (x * cos θ - y * sin θ,
                     x * sin θ + y * cos θ)
        (x',y') = case shape of
          Square -> (fromIntegral x, fromIntegral y)
          Hexagon -> let (a,b,c) = (x,(-x)-y,y) in
            (fromIntegral a * 6 / 8, sqrt 3 / 4 * fromIntegral (c-b))

-- This takes a hue value and returns an RGB colour
-- this should produce colours sufficiently different
-- from each other, with minimal collisions
niceColour :: Integer -> Integer -> Colour -> (Float, Float, Float)
niceColour initial change col = case i of
          0 -> (1,t,0)
          1 -> (q,1,0)
          2 -> (0,1,t)
          3 -> (0,1,t)
          4 -> (t,0,1)
          5 -> (1,0,q)
          _ -> error "Invalid colour"
  where hue = (col*change+(initial - change)) `mod` 360
        i = hue `div` 60
        f = (fromIntegral hue/60) - fromIntegral i
        q = 1 - f
        t = 1 - q
