module Cuboid (
-- * Functions
    Cuboid.empty,
    get,
    has,
    set,
    getAllPositions,

-- * Types
    Cuboid,
) where

-- | Imports

import Data.Map.Strict as M (Map, empty, lookup, insert, member)
import Common

-- | Types

data Cuboid a = C (Map Pos a) Size
    deriving Show

-- | Functions

empty :: Size -> Cuboid a
empty = C M.empty

get :: Pos -> Cuboid a -> Maybe a
get pos (C m _) = M.lookup pos m

has :: Pos -> Cuboid a -> Bool
has pos (C m _) = M.member pos m

set :: Pos -> a -> Cuboid a -> Cuboid a
set pos v (C m size) = C (M.insert pos v m) size

getAllPositions :: Size -> [Pos]
getAllPositions (x,y,z) = [(px,py,pz) | px <- [0..(z-1)], py <- [0..(y-1)], pz <- [0..(x-1)]]
