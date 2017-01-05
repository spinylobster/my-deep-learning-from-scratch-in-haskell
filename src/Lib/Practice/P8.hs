{-# LANGUAGE FlexibleContexts #-}

module Lib.Practice.P8 (
    f8
) where

import Numeric.LinearAlgebra
import Lib.Utility

f8 = do
    putStrLn "f8"

y = (2><10) [0.1, 0.05, 0.6, 0.0, 0.05, 0.1, 0.0, 0.1, 0.0, 0.0,   0.1, 0.05, 0.1, 0.0, 0.05, 0.1, 0.0, 0.6, 0.5, 0.0] :: Matrix Double
t = asRow $ fromList [0,0,1,0,0,0,0,0,0,0] :: Matrix Double
meanSquaredError y t = map ((/ 2) . sumElements) (toRows $ (y - t) ** 2)
crossEntropyError y t = map (negate . sumElements) $ toRows $ cmap log (y + delta) * t
    where delta = 1e-7

function1 x = (0.01 * x ** 2) + 0.1 * x
function1_xs = fromList [0,0.1..20.0] :: Vector Double
function1_ys = cmap function1 function1_xs
function2 :: Vector Double -> Double
function2 xs = sumElements $ xs ** 2
function2_initXs = fromList [-3.0, 4.0] :: Vector Double
