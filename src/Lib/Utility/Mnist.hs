{-# LANGUAGE Rank2Types, FlexibleContexts, UndecidableInstances, FlexibleInstances, ScopedTypeVariables, DataKinds #-}

module Lib.Utility.Mnist
(
    getMnistData, miniBatch,
    TrainingData
) where

import Lib.Utility.Mnist.IO
import qualified Numeric.LinearAlgebra as HMatrix
import qualified Numeric.LinearAlgebra.Static as Static
import Numeric.LinearAlgebra.Devel
import Control.Monad.ST
import Data.Array.ST
import Control.Arrow
import Data.Proxy
import GHC.TypeLits
import Control.Monad.Trans.Maybe
import System.Random

miniBatch :: forall n m. (KnownNat n, KnownNat m) => TrainingData n -> MaybeT IO (TrainingData m)
miniBatch (images, labels) = MaybeT $ do
    let allSize = fromIntegral $ natVal (Proxy :: Proxy n)
    let size = fromIntegral $ natVal (Proxy :: Proxy m)
    gen <- newStdGen
    let rs = HMatrix.idxs $ take size $ randomRs (0, allSize - 1) gen
    let extr = (HMatrix.Pos rs, HMatrix.All)
    return $ do
        images' <- (Static.create $ Static.unwrap images HMatrix.?? extr) :: Maybe (Static.L m 784)
        labels' <- (Static.create $ Static.unwrap labels HMatrix.?? extr) :: Maybe (Static.L m 10)
        return (images', labels')
