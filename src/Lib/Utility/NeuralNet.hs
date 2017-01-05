{-# LANGUAGE Rank2Types    #-}

module Lib.Utility.NeuralNet where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Devel
import Control.Monad.ST
import Data.Array.ST
import Control.Arrow

type X = Matrix Double
type Y = Matrix Double
type T = Y -- one-hot表現
type W = Matrix Double
type B = Vector Double
type H = X -> X
type NeuralParam = (W, B, IndexOf Matrix, IndexOf Matrix)
type LossF = T -> Y -> Double
type HyperP = (Double, Int)
type PInitF = IndexOf Matrix -> IO NeuralParam
type TrainingData = (X, T)

data NeuralNet = NeuralNet {
    params :: [NeuralParam],
    activationF :: H,
    outputActivationF :: H,
    depth :: Int
}

calcLayer :: H -> X -> NeuralParam -> X
calcLayer aF x (w, b, _, _) = aF a
    where xw    = x <> w
          a     = xw + asRow b

predict :: NeuralNet -> X -> Y
predict (NeuralNet ps aF oAF depth) x = calcLast intermediateA lastP
    where ps' = take (depth - 1) ps
          lastP = last ps
          intermediateA = foldl (calcLayer aF) x ps'
          calcLast = calcLayer oAF

accuracy :: NeuralNet -> TrainingData -> Double -> Double
accuracy net (x, t) tRows = (realToFrac $ length corrects) / tRows
    where   y = predict net x
            [results, answers] = map (map maxIndex . toRows) [y, t]
            corrects = filter id $ zipWith (==) results answers


trainingNeuralNet :: Double -> LossF -> TrainingData -> Int -> NeuralNet -> NeuralNet
trainingNeuralNet learningRate lossF (dataX, dataT) dataXRows (NeuralNet ps aF oAF depth) = NeuralNet afterTrainingParams aF oAF depth
    where afterTrainingParams = renewParam 0 ps
          renewPs :: [NeuralParam] -> Int -> Bool -> X -> [NeuralParam]
          renewPs ((w,b,sW,sB):xs) 0 wOrb x = if wOrb then (x,b,sW,sB):xs else (w,flatten x,sW,sB):xs
          renewPs (before:xs) layer wOrb x = before : renewPs xs (layer-1) wOrb x
          renewParam :: Int -> [NeuralParam] -> [NeuralParam]
          -- TODO ↓この辺りがおかしい。部分的に変更したパラメータで予測を計算し直したいだけなのに。Stateモナド?
          renewParam layer [(w, b, sW, sB)] =
              let
                  gradW = calcGradient (renewPs ps layer True) sW w
                  b' = asRow b
                  gradB = calcGradient (renewPs ps layer False) sB b'
                  newW = w - matrix 1 [learningRate] * gradW
                  newB = flatten $ b' - matrix 1 [learningRate] * gradB
              in
                  [(newW, newB, sW, sB)]
          renewParam layer ((w,b,sW,sB):rest@(x:xs)) =
              let
                  gradW = calcGradient (renewPs ps layer True) sW w
                  b' = asRow b
                  gradB = calcGradient (renewPs ps layer False) sB b'
                  newW = w - matrix 1 [learningRate] * gradW
                  newB = flatten $ b' - matrix 1 [learningRate] * gradB
              in
                  (newW, newB, sW, sB) : renewParam (layer+1) rest
          calcGradient :: (X -> [NeuralParam]) -> IndexOf Matrix -> X -> X
          calcGradient renewF (rows, columns) x = runSTMatrix $ do
              x' <- thawMatrix x
              let grad = newMatrix 0 rows columns
              let allIndex = [(r,c) | r <- [0..rows-1], c <- [0..columns-1]]
              foldr (\(r, c) grad -> do
                  let h = 1e-4
                  let rePredict newX = predict (NeuralNet (renewF newX) aF oAF depth) dataX
                  x <- readMatrix x' r c
                  grad' <- grad
                  writeMatrix x' r c $ x+h
                  x'' <- freezeMatrix x'
                  let fxHPlus = crossEntropyError dataT (rePredict x'')
                  writeMatrix x' r c $ x-h
                  x'' <- freezeMatrix x'
                  let fxHMinus = crossEntropyError dataT (rePredict x'')
                  let delta = (fxHPlus - fxHMinus) / (2 * h)
                  writeMatrix x' r c x
                  writeMatrix grad' r c delta
                  return grad'
                  ) grad allIndex

initNet :: [IndexOf Matrix] -> PInitF -> H -> H -> IO NeuralNet
initNet wSizes paramInitF aF oAF = result
    where result = do
              initParams <- getInitParamsAction
              return $ NeuralNet initParams aF oAF depth
          depth = length wSizes
          getInitParamsAction = f wSizes
          f [wSize@(rows, columns)] = do
              w <- randn rows columns
              return  [(w, columns |> repeat 0, wSize, (1, columns))]
          f (x:xs) = do
              [w] <- f [x]
              ws <- f xs
              return $ w : ws

stepF x = if x > 0 then 1.0 else 0.0
sigmoid :: H
sigmoid = cmap f
    where f x = 1 / (1 + exp(-x))
relu :: Vector Double -> Double
relu = maxElement
softmax :: Int -> H
softmax aCols a = fromRows $ zipWith (curry devF) expA sumExpA
    where maxs :: [Vector Double]
          maxs          = map ((aCols |>) . repeat . maxElement) xs
          xs            = toRows a
          xs'           = zipWith (-) xs maxs
          expA          = map (cmap exp) xs'
          sumExpA       = map sumElements expA
          devF :: (Vector Double, Double) -> Vector Double
          devF (x, sum) = cmap (/sum) x
meanSquaredError :: LossF
meanSquaredError t y = sumElements $ (y-t) ** 2
crossEntropyError :: LossF
crossEntropyError t y = -sum
    where logy = cmap log y
          sum = sumElements $ t * logy
