module Cuboid (
-- * Functions
    emptyCube,
    get,
    has,
    set,
    setM,
    deleteCandidate,
    getCandidates,

-- * Types
    Cuboid,
) where

import Data.Map.Strict as M (Map, empty, lookup, insert, member)
import Data.Set as S (Set, fromList, delete, elems)
import Common
import Control.Monad (mfilter)

data Cuboid a = C (Set Pos) (Map Pos a) Size
    deriving Show

emptyCube :: Size -> Cuboid a
emptyCube size = C (fromList [(px,py,pz) | px <- [0..(z-1)], py <- [0..(y-1)], pz <- [0..(x-1)]]) empty size
    where (x,y,z) = size

get :: Pos -> Cuboid a -> Maybe a
get pos (C _ m _) = M.lookup pos m

has :: Pos -> Cuboid a -> Bool
has pos (C _ m _) = M.member pos m

setM :: Pos -> a -> Cuboid a -> Maybe (Cuboid a)
setM pos v (C l m size) = mfilter (const $ isValidPos size pos) $ Just $ C l (M.insert pos v m) size

set :: Pos -> a -> Cuboid a -> Cuboid a
set pos v (C l m size) = C l (M.insert pos v m) size

deleteCandidate :: Pos -> Cuboid a -> Cuboid a
deleteCandidate pos (C l m size) = C (S.delete pos l) m size

getCandidates :: Cuboid a -> [Pos]
getCandidates (C l _ _) = S.elems l

isValidPos :: Size -> Pos -> Bool
isValidPos (x,y,z) (px,py,pz) =
    px >= 0 && px < x &&
    py >= 0 && py < y &&
    pz >= 0 && pz < z
