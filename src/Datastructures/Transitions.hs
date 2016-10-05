-- The different kinds of transitions than an ant can do
-- If you want to define a kind of ant that can do more
-- complicated behaviour than what's give here,
-- you might need to change this, otherwise you just
-- need to look at this

module Datastructures.Transitions where

import Datastructures.Cells

data Transition a = Transition { onColour :: Colour
                               , turnDirection :: a}
  deriving (Show, Eq)

data Direction = N | E | S | W -- Up, Down, Left and Right
  deriving (Eq, Ord, Bounded, Enum, Show)

-- A modified version of `pred` that loops around.  (The laws for `Enum`
-- dictate that `pred N = undefined` by default).
pred' :: Direction -> Direction
pred' N = W
pred' d = pred d

-- A modified version of `succ` that loops around.  (The laws for `Enum`
-- dictate that `succ W = undefined` by default).
succ' :: Direction -> Direction
succ' W = N
succ' d = succ d

data SquareTurn = L | R -- Left and Right. Feel free to add a U-turn and
                        -- do nothing if you want them for an extension.
                        -- It shouldn't break anything (except perhaps your
                        -- own code) 
  deriving (Show, Eq, Enum, Bounded)

data HexTurn = L2 | L1 | None | R1 | R2 | U -- None is for no turn,
                                            -- U is for U-turn
  deriving (Show, Eq, Enum, Bounded)

data HexDirection = Up | RU | RD | Dn | LD | LU
  -- Up, Right-up, Right-down, Down, Left-down, Left-up 
  deriving (Eq, Ord, Enum, Bounded, Show)
