module Cuboid2 (
-- * Functions
    emptyCube,
    get,
    set,

-- * Types
    Cuboid,
) where

import Data.Map (Map, empty)
import Transform (Size, Pos)

data Cuboid a = C [Pos] (Map Pos a)

emptyCube :: Size -> Cuboid a
emptyCube (x,y,z) = C [(px,py,pz) | px <- [0..(z-1)], py <- [0..(y-1)], pz <- [0..(x-1)] empty

-- | Safe
get :: Pos -> Cuboid a -> Maybe a
get pos (C _ m) = lookup pos m

-- | Unsafe
set :: Pos -> a -> Cuboid a -> Cuboid a
set pos v (C _ m) = insert pos v m
