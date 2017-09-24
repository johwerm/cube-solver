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

import Data.Maybe (isJust)
import Data.Array
import Control.Monad (join)
import Common

-- | Types

data Cuboid a = C (Array Int (Maybe a)) Size

-- | Functions

empty :: Size -> Cuboid a
empty size = C (listArray (0, (length $ getAllPositions size) - 1) [Nothing | _ <- getAllPositions size]) size

-- | Unsafe
has :: Pos -> Cuboid a -> Bool
has pos = isJust . get pos

get :: Pos -> Cuboid a -> Maybe a
get (x,y,z) (C c (sx,sy,sz)) = c ! (x*sy*sz+y*sz+z)

set :: Pos -> a -> Cuboid a -> Cuboid a
set (x,y,z) v (C c size) = C (c // [(x*sy*sz+y*sz+z,Just v)]) size
    where (sx,sy,sz) = size

getAllPositions :: Size -> [Pos]
getAllPositions (x,y,z) = [(px,py,pz) | px <- [0..(z-1)], py <- [0..(y-1)], pz <- [0..(x-1)]]
