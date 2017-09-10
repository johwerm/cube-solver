module Cuboid (
-- * Functions
    emptyCube,

-- * Types
    Cuboid,
) where

import Data.Vector
import Transform (Size, Pos)

type Cuboid a = Vector (Vector (Vector a))

emptyCube :: a -> Size -> Cuboid a
emptyCube def (x,y,z) = fromList [fromList [fromList [def | _ <- [0..(z-1)]] | _ <- [0..(y-1)]] | _ <- [0..(x-1)]]

get :: Pos -> Cuboid a -> Maybe a
get (x,y,z) c = undefined
