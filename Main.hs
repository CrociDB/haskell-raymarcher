module Main where

import System.Console.ANSI
import Control.Monad

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
    renderScreen 10 50
