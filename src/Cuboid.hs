module Cuboid (
-- * Functions
    emptyCube,
    get,
    set,

-- * Types
    Cuboid,
) where

import Control.Monad (join)
import Data.Vector
import Transform (Size, Pos)

type Cuboid a = Vector (Vector (Vector a))

emptyCube :: a -> Size -> Cuboid a
emptyCube def (x,y,z) = fromList [fromList [fromList [def | _ <- [0..(z-1)]] | _ <- [0..(y-1)]] | _ <- [0..(x-1)]]

-- | Safe
get :: Pos -> Cuboid a -> Maybe a
get (x,y,z) c = c !? x >>= (flip (!?) y) >>= (flip (!?) z)

-- | Unsafe
set :: Pos -> a -> Cuboid a -> Cuboid a
set (x,y,z) v c = c // [(x,cyz // [(y,cz // [(z,v)])])]
    where cyz = c ! x
          cz = cyz ! y
