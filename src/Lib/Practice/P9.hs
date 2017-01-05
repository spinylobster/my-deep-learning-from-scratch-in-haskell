module Lib.Practice.P9 (
    f9
) where

import Lib.Utility
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Devel
import Control.Applicative
import Data.Foldable
import Control.Monad

f9 :: IO ()
f9 = toIO $ do
    (training, test) <- getMnistData
    toMaybeT $ putStrLn "load complete"
    net <- MaybeT $ fmap Just initNetA
    toMaybeT $ putStrLn "training start"
    let trainingF' = trainingF 0.1 training
    net100 <- foldM (\net _ -> trainingF' net) net [1..1]
    let testBatchSize = 1 :: Double
    testBatch <- miniBatch (truncate testBatchSize) teSize test
    toMaybeT $ print $ accuracy net testBatch testBatchSize
    net200 <- foldM (\net _ -> trainingF' net) net [1..1]
    toMaybeT $ print $ accuracy net200 testBatch testBatchSize
    toMaybeT $ putStrLn "training complete"
    return ()

test' = toIO $ do
    (training, test) <- getMnistData
    toMaybeT $ putStrLn "load complete"
    net <- MaybeT $ fmap Just initNetA
    toMaybeT $ putStrLn "training start"
    testBatch <- miniBatch 1 teSize test
    net'@(NeuralNet [(w1,b1,_,_),(w2,b2,_,_)] _ _ _) <- trainingF 0.1 training net
    toMaybeT $ print $ size w2
    toMaybeT $ print $ accuracy net' testBatch 1.0
    toMaybeT $ putStrLn "training complete"
    return ()

minibatch' = toIO $ do
    (training, test) <- getMnistData
    (x, t) <- miniBatch 100 trSize training
    toMaybeT $ print $ size x

trainingF :: Double -> TrainingData -> NeuralNet -> MaybeT IO NeuralNet
trainingF learningRate allData net = do
    let trainingDataSize = 1
    trainingData <- miniBatch trainingDataSize trSize allData
    let net' = trainingNeuralNet learningRate crossEntropyError trainingData trainingDataSize net
    return net'

initNetA = initNet [(784,50),(50, 10)] pInitF sigmoid (softmax 10)
    where pInitF :: PInitF
          pInitF wSize@(rows, columns) = do
              w <- randn rows columns
              return (0.01 * w, columns |> repeat 0, wSize, (1, columns))

toIO :: MaybeT IO () -> IO ()
toIO (MaybeT m) = result
    where result = do
              result' <- m
              forM_ result' return

toMaybeT :: IO a -> MaybeT IO a
toMaybeT x = MaybeT $ fmap Just x
