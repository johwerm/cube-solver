module Transform (
-- * Functions
    transform,
    transforms,

-- * Types
    Pos,
    Size,
    Transform
) where

import Data.Matrix

type Transform = (Translation, Rotation)
type Rotation = Matrix Integer
type Translation = Pos

type Size = Pos
type Pos = (Integer, Integer, Integer)

transforms :: Size -> [Transform]
transforms (x,y,z) = [((px,py,pz), r) | px <- [0..(x-1)], py <- [0..(y-1)], pz <- [0..(z-1)], r <- rotationMatrices]

transform :: Pos -> Transform -> Pos
transform pos (trans,rot) = translate (rotate pos rot) trans

translate :: Pos -> Translation -> Pos
translate (x,y,z) (tx,ty,tz) = (x+tx,y+ty,z+tz)

rotate :: Pos -> Rotation -> Pos
rotate pos rot = matrixToPos $ rot * posToMatrix pos

posToMatrix :: Pos -> Matrix Integer
posToMatrix (x,y,z) = fromList 3 1 [x,y,z]

matrixToPos :: Matrix Integer -> Pos
matrixToPos m = (x,y,z)
    where [x,y,z] = toList m

rotationMatrices :: [Rotation]
rotationMatrices = map fromLists $ [
    [[1,0,0],
     [0,1,0],
     [0,0,1]],
    [[1,0,0],
     [0,0,-1],
     [0,1,0]],
    [[1,0,0],
     [0,-1,0],
     [0,0,-1]],
    [[1,0,0],
     [0,0,1],
     [0,-1,0]],
    [[0,-1,0],
     [1,0,0],
     [0,0,1]],
    [[0,0,1],
     [1,0,0],
     [0,1,0]],
    [[0,1,0],
     [1,0,0],
     [0,0,-1]],
    [[0,0,-1],
     [1,0,0],
     [0,-1,0]],
    [[-1,0,0],
     [0,-1,0],
     [0,0,1]],
    [[-1,0,0],
     [0,0,-1],
     [0,-1,0]],
    [[-1,0,0],
     [0,1,0],
     [0,0,-1]],
    [[-1,0,0],
     [0,0,1],
     [0,1,0]],
    [[0,1,0],
     [-1,0,0],
     [0,0,1]],
    [[0,0,1],
     [-1,0,0],
     [0,-1,0]],
    [[0,-1,0],
     [-1,0,0],
     [0,0,-1]],
    [[0,0,-1],
     [-1,0,0],
     [0,1,0]],
    [[0,0,-1],
     [0,-1,0],
     [-1,0,0]],
    [[0,0,-1],
     [0,1,0],
     [1,0,0]],
    [[0,1,0],
     [0,0,1],
     [1,0,0]],
    [[0,0,1],
     [0,-1,0],
     [1,0,0]],
    [[0,-1,0],
     [0,0,1],
     [-1,0,0]],
    [[0,0,1],
     [0,1,0],
     [-1,0,0]],
    [[0,1,0],
     [0,0,-1],
     [-1,0,0]]]
