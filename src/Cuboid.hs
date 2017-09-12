module Cuboid (
-- * Functions
    emptyCube,
    get,
    set,
    deleteCandidate,
    getCandidates,

-- * Types
    Cuboid,
) where

import Data.Map.Strict as M (Map, empty, lookup, insert)
import Data.Set as S (Set, fromList, delete, elems)
import Common

data Cuboid a = C (Set Pos) (Map Pos a)
    deriving Show

emptyCube :: Size -> Cuboid a
emptyCube (x,y,z) = C (fromList [(px,py,pz) | px <- [0..(z-1)], py <- [0..(y-1)], pz <- [0..(x-1)]]) empty

get :: Pos -> Cuboid a -> Maybe a
get pos (C _ m) = M.lookup pos m

set :: Pos -> a -> Cuboid a -> Cuboid a
set pos v (C l m) = C l $ M.insert pos v m

deleteCandidate :: Pos -> Cuboid a -> Cuboid a
deleteCandidate pos (C l m) = C (S.delete pos l) m

getCandidates :: Cuboid a -> [Pos]
getCandidates (C l _) = S.elems l
