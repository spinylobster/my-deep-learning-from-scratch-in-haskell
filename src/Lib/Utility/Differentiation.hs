module Lib.Utility.Differentiation (
    numericalGradient
 ) where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Devel

h = 1e-4

type F = X -> Double
type X = Matrix Double
numericalGradient :: F -> X -> X
numericalGradient f xs = runSTMatrix $ do
    xs' <- thawMatrix xs
    let (rows, columns) = size xs
    let grad = newMatrix 0 rows columns
    let allIndex = [(r,c) | r <- [0..rows-1], c <- [0..columns-1]]
    foldr (\(r, c) grad -> do
        x <- readMatrix xs' r c
        grad' <- grad
        writeMatrix xs' r c $ x+h
        xs'' <- freezeMatrix xs'
        let fxHPlus = f xs''
        writeMatrix xs' r c $ x-h
        xs'' <- freezeMatrix xs'
        let fxHMinus = f xs''
        let delta = (fxHPlus - fxHMinus) / (2 * h)
        writeMatrix xs' r c x
        writeMatrix grad' r c delta
        return grad'
        ) grad allIndex
