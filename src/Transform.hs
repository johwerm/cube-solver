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
import Common

data Transform = T Translation Rotation

instance Show Transform where
    show (T t r) = "Translation:\n" ++ show t ++ "\n" ++
                    "Rotation:\n" ++ show r

type Rotation = Matrix Int
type Translation = Pos

transforms :: [Pos] -> [Transform]
transforms emptyposs = [T pos r | pos <- emptyposs, r <- rotationMatrices]

transform :: Pos -> Transform -> Pos
transform pos (T trans rot) = translate (rotate pos rot) trans

translate :: Pos -> Translation -> Pos
translate (x,y,z) (tx,ty,tz) = (x+tx,y+ty,z+tz)

rotate :: Pos -> Rotation -> Pos
rotate pos rot = matrixToPos $ rot * posToMatrix pos

posToMatrix :: Pos -> Matrix Int
posToMatrix (x,y,z) = fromList 3 1 [x,y,z]

matrixToPos :: Matrix Int -> Pos
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
     [0,1,0],
     [1,0,0]],
    [[0,1,0],
     [0,0,1],
     [1,0,0]],
    [[0,0,1],
     [0,-1,0],
     [1,0,0]],
    [[0,-1,0],
     [0,0,-1],
     [1,0,0]],
    [[0,0,-1],
     [0,-1,0],
     [-1,0,0]],
    [[0,-1,0],
     [0,0,1],
     [-1,0,0]],
    [[0,0,1],
     [0,1,0],
     [-1,0,0]],
    [[0,1,0],
     [0,0,-1],
     [-1,0,0]]]
