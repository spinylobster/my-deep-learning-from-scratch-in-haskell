{-# LANGUAGE DataKinds, FlexibleContexts #-}

module Lib.Practice.P9 (
    f9
) where

import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra.Static
import Lib.Utility
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import GHC.TypeLits

f9 = downMaybeTIO f9'
f9' :: MaybeT IO ()
f9' = do
    mnist@(tr@(trX, trT), te@(teX, teT)) <- getMnistData
    net <- liftIO (initTestNet 0.01 :: IO (MnistTwoLayerNet 50 100 784 10))
    showAccuracy te net
    net' <- train10000 mnist net
    --net' <- train100 net train'
    --showAccuracy net'
    return ()

showNetParam (MnistTwoLayerNet af1 relu af2 _) = b1
    where (Layer param2 _ _) = af2
          (Layer param1 _ _) = af1
          ((w2, b2), _, _) = param2
          ((w1, b1), _, _) = param1
train10000 mnist net = foldM (\ net' _ -> train600 mnist net') net [1..16]
train600 (tr, te) net = do
    net' <- foldM (\ net' _ -> train tr net') net [1 .. 600]
    showAccuracy te net'
    liftIO $ print $ showNetParam net'
    return net'

showAccuracy te net = do
    (batchX, batchT) <- miniBatch te
    MaybeT $ fmap Just $ print $ accuracy net batchX batchT
train tr net = do
    (batchX, batchT) <- miniBatch tr
    return $ training' net batchX batchT

training' (MnistTwoLayerNet af1 relu af2 updater) x t = MnistTwoLayerNet af1' relu af2' updater'
    where ff layer x = case layer of (Layer _ ff _) -> ff x
          bf layer state = case layer of (Layer _ _ bf) -> bf state
          (y1, af1s) = ff af1 x
          (y2, relus) = ff relu y1
          (y3, af2s) = ff af2 y2
          swl = softmaxWithLossLayer t
          (y4, swls) = ff swl y3
          resultMaybe = do
              let (dx1, _) = bf swl swls 1
              let (dx2, (p2, _, dwdbMaybe2)) = bf af2 af2s dx1
              dp2 <- dwdbMaybe2
              let (dx3, _) = bf relu relus dx2
              let (_, (p1, _, dwdbMaybe1)) = bf af1 af1s dx3
              dp1 <- dwdbMaybe1
              let ((af1s', af2s'), u') = update updater (p1, p2) (dp1, dp2)
              return (affineLayer af1s', affineLayer af2s', u')
          (af1', af2', updater') = case resultMaybe of
              Nothing -> error "Something wrong"
              Just x -> x

testAffine = y
    where (Layer s f bf) = affineLayer (w, b) :: Layer 2 2 3 (AffineState 2 2 3)
          (y, s') = f x
          (dx, (_, _, Just (dw, db))) = bf s' (matrix [1..6] :: L 2 3)
          x = matrix [0,0,1,1] :: L 2 2
          w = matrix [0,0,0,10,10,10] :: L 2 3
          b = vec3 1 2 3
testRelu = dy
    where (Layer s f bf) = reluLayer
          (y, state) = f (matrix [1.0,-0.5,-2.0,3.0] :: L 2 2)
          (dy, _) = bf state (matrix [2.0,5.5,-2.0,3.0] :: L 2 2)
testSoftmax = softmax (matrix [1010, 1000, 990] :: L 1 3)
testCrossEntropyError = crossEntropyError t y
    where t = matrix [0,0,1,0,0,0,0,0,0,0] :: L 1 10
          y = matrix [0.1,0.05,0.6,0.0,0.05,0.1,0.0,0.1,0.0,0.0] :: L 1 10
