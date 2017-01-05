module Lib.Practice.P3_1 (
    f3_1
) where

import Lib.Practice.P3 (Input( Fire, NotFire ), input2Int, inputs)
import Numeric.LinearAlgebra

f3_1 = showOutput ("XOR", gXor)

gAnd = Single (SNeuron (vector [0.5, 0.5]) (-0.7))
gNand = Single (SNeuron (vector [-0.5, -0.5]) 0.7)
gOr = Single (SNeuron (vector [0.5, 0.5]) (-0.2))
gXor = Multi MNeuron { inputLayer = [gNand, gOr], intermediateLayer = gAnd }

showOutput (name, gate) = do
    putStrLn $ name ++ "ゲート"
    mapM_ (showOutput' gate) inputs
    putStrLn ""
    where
        showOutput' gate input = do
            let result = input --> gate
            putStrLn $ show input ++ " -> " ++ show result

data SNeuron = SNeuron {
    weight :: Vector R,
    bias :: R
} deriving Show

data MNeuron = MNeuron {
    inputLayer :: [Neuron],
    intermediateLayer :: Neuron
} deriving Show

data Neuron = Single SNeuron | Multi MNeuron deriving Show
(-->) :: [Input] -> Neuron -> Input
inputs --> Single SNeuron { weight = w, bias = b } = result
    where inputV    = fromList $ map input2Int inputs
          sum'      = sumElements $ inputV * w
          result'   = b + sum'
          result    = case () of
            _ | result' >= 0 -> Fire 
            _                -> NotFire
inputs --> Multi MNeuron { inputLayer = inpLs, intermediateLayer = intL } = result
    where inputs'   = map (inputs -->) inpLs
          result    = inputs' --> intL