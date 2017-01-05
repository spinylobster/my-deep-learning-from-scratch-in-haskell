module Lib.Practice.P3 where

import Control.Monad
import Numeric.LinearAlgebra

f3 = do
    let gates = [("AND", gAnd), ("NAND", gNand), ("OR", gOr)]
    mapM_ showOutput gates

gAnd = Neuron { weight = vector [0.5, 0.5], bias = -0.7 }
gNand = Neuron { weight = vector [-0.5, -0.5], bias = 0.7 }
gOr = Neuron { weight = vector [0.5, 0.5], bias = -0.2 }

inputs = replicateM 2 [NotFire, Fire]
showOutput (name, gate) = do
    putStrLn $ name ++ "ゲート"
    mapM_ (showOutput' gate) inputs
    putStrLn ""
    where
        showOutput' gate input = do
            let result = input --> gate
            putStrLn $ show input ++ " -> " ++ show result

data Neuron = Neuron {
    weight :: Vector R,
    bias :: R
} deriving Show

data Input = Fire | NotFire deriving Show
input2Int Fire = 1
input2Int NotFire = 0

(-->) = output
output :: [Input] -> Neuron -> Input
output inputs Neuron { weight = w, bias = b } = result
    where inputV    = fromList $ map input2Int inputs
          sum'      = sumElements $ inputV * w
          result'   = b + sum'
          result    = case () of
            _ | result' >= 0    -> Fire 
            otherwise           -> NotFire
