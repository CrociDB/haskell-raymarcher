module Main where

import System.Console.ANSI
import Control.Monad

-- 3D Math

data Vector = Vector3 Float Float Float 
    deriving (Show)

vecGetX (Vector3 x _ _) = x
vecGetY (Vector3 _ y _) = y
vecGetZ (Vector3 _ _ z) = z

vecAdd (Vector3 x y z) (Vector3 x' y' z') = Vector3 (x + x') (y + y') (z + z')
vecSub (Vector3 x y z) (Vector3 x' y' z') = Vector3 (x - x') (y - y') (z - z')
vecMul (Vector3 x y z) f = Vector3 (x * f) (y * f) (z * f)
vecLength (Vector3 x y z) = sqrt (x*x + y*y + z*z)
vecNormalize (Vector3 x y z) = Vector3 (x / length) (y / length) (z / length)
    where length = vecLength (Vector3 x y z)

-- Screen Rendering specifics

renderScreen w h = do
    renderLine w h
    when (w > 0) $ renderScreen (w - 1) h

renderLine x y = do
    getPixelAt x y
    when (y > 0) $ renderLine x (y - 1)

getPixelAt x y = do
    setCursorPosition x y
    putStrLn $ "-"

main :: IO ()
main = do
    clearScreen
    renderScreen 25 120
