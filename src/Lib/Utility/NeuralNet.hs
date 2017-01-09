{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib.Utility.NeuralNet where

import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra.Static
import Numeric.LinearAlgebra.Devel
import Control.Monad.State
import Control.Arrow
import GHC.TypeLits
import Data.Proxy

data Layer (il :: Nat) (is :: Nat) (wc :: Nat) (state :: *) where
    Layer :: state -> (X il is -> (Y il wc, state)) -> (state -> Delta il wc -> (DX il is, state)) -> Layer il is wc state

type ReluState il wc = Maybe (L il wc)
reluLayer :: forall il wc. (KnownNat il, KnownNat wc) =>
   Layer il wc wc (ReluState il wc)
reluLayer = Layer Nothing forwardF backwardF
    where forwardF x = (lt0, Just lt0)
              where (Just lt0) = create $ LA.cmap (\ e -> if e <= 0 then 0 else e) $ extract x
          backwardF Nothing _ = error "State is Nothing"
          backwardF (Just lt0) d = (zipWithM' (\ e1 e2 -> if e1 == 0 then 0 else e2) lt0 d, Just lt0)

type SigmoidState il wc = Maybe (L il wc)
sigmoidLayer :: forall il wc. (KnownNat il, KnownNat wc) =>
   Layer il wc wc (ReluState il wc)
sigmoidLayer = Layer Nothing forwardF backwardF
    where forwardF x = (y, Just y)
              where y = 1 / (1 + exp (-x))
          backwardF Nothing _ = error "State Y is Nothing"
          backwardF (Just s) d = (d * (1 - s) * s, Just s)

type AffineState il is wc = (NeuralParam is wc, Maybe (X il is), Maybe (NeuralParam is wc))
affineLayer :: forall il is wc. (KnownNat il, KnownNat is, KnownNat wc) =>
    NeuralParam is wc -> Layer il is wc (AffineState il is wc)
affineLayer param@(w, b) = Layer (param, Nothing, Nothing) forwardF backwardF
    where forwardF x = (x <> w + v2m b, (param, Just x, Nothing))
          backwardF (_, Nothing, _) _ = error "State X is Nothing"
          backwardF (param@(w, b), Just x, _) d = (dx, (param, Just x, Just (dw, db)))
              where dx = d <> tr w
                    dw = tr x <> d
                    db = unrow $ (1 :: L 1 il) <> d

type SoftmaxWithLossState il os = (T il os, Maybe (Y il os), Maybe (Loss il 1))
softmaxWithLossLayer :: forall il os. (KnownNat il, KnownNat os) =>
    T il os -> Layer il os 1 (SoftmaxWithLossState il os)
softmaxWithLossLayer t = Layer (t, Nothing, Nothing) forwardF backwardF
    where forwardF x = (loss, (t, Just y, Just loss))
              where y = softmax x
                    loss = crossEntropyError t y
          backwardF (_, Nothing, _) d = error "State Y is Nothing"
          backwardF state@(t, Just y, _) d = ((y - t) / fromIntegral il, state)
              where (il, _) = size d

class NeuralNet (a :: Nat -> Nat -> Nat -> *) where
    predict :: forall il is os. (KnownNat il, KnownNat is, KnownNat os)
         => a il is os -> X il is -> Prediction il os
    loss :: forall il is os. (KnownNat il, KnownNat is, KnownNat os)
         => a il is os -> X il is -> Loss il os
    accuracy :: forall il is os. (KnownNat il, KnownNat is, KnownNat os)
         => a il is os -> X il is -> T il os -> Double
    gradients :: forall il is os. (KnownNat il, KnownNat is, KnownNat os)
         => a il is os -> X il is -> T il os -> ()
    training :: forall il is os. (KnownNat il, KnownNat is, KnownNat os)
         => a il is os -> X il is -> T il os -> a il is os

    accuracy a x t = LA.sumElements (extract t1f0) / fromIntegral (size t1f0)
        where y = predict a x
              (y', t') = (maxIndex' y, maxIndex' t)
              t1f0 = uncol $ zipWithM' (\ e1 e2 -> if e1 == e2 then 1 else 0) y' t'

data MnistTwoLayerNet (wc :: Nat) (il :: Nat)  (is :: Nat) (os :: Nat) where
    MnistTwoLayerNet ::
        Layer il is wc (AffineState il is wc)
     -> Layer il wc wc (ReluState il wc)
     -> Layer il wc os (AffineState il wc os)
     -> AdaGrad wc il is os
     -> MnistTwoLayerNet wc il is os
instance NeuralNet (MnistTwoLayerNet 50) where
    predict (MnistTwoLayerNet af1 relu af2 _) x = y3
        where (y1, af1s) = case af1 of (Layer _ ff _) -> ff x
              (y2, relus) = case relu of (Layer _ ff _) -> ff y1
              (y3, af2s) = case af2 of (Layer _ ff _) -> ff y2
    loss = undefined
    gradients = undefined
    training = undefined

class Updater (a :: Nat -> Nat -> Nat -> Nat -> *) where
    update :: forall wc il is os. (KnownNat wc, KnownNat il, KnownNat is, KnownNat os)
         => a wc il is os
         -> (NeuralParam is wc, NeuralParam wc os)
         -> (NeuralParam is wc, NeuralParam wc os)
         -> ((NeuralParam is wc, NeuralParam wc os), a wc il is os)

data SGD (wc :: Nat) (il :: Nat) (is :: Nat) (os :: Nat) where
    SGD :: Double -> SGD wc il is os
instance Updater SGD where
    update (SGD lr) (p1, p2) (dp1, dp2) = ((newParam lr p1 dp1, newParam lr p2 dp2), SGD lr)

newParam :: forall is wc. (KnownNat is, KnownNat wc) => Double
    -> NeuralParam is wc -> NeuralParam is wc -> NeuralParam is wc
newParam lr (w, b) (dw, db) = (w - lr_w * dw, b - lr_b * db)
    where (lr_w, lr_b) = (realToFrac lr, realToFrac lr)

data AdaGrad (wc :: Nat) (il :: Nat) (is :: Nat) (os :: Nat) where
    AdaGrad :: Double -> (NeuralParam is wc, NeuralParam wc os) -> AdaGrad wc il is os
instance Updater AdaGrad where
    update (AdaGrad lr (h1, h2)) (p1, p2) (dp1, dp2) = (result, AdaGrad lr newH)
        where result = (newParam lr p1 dp1', newParam lr p2 dp2')
              (dp1', dp2') = (f' newH1 dp1, f' newH2 dp2)
                  where f h dx = dx / sqrt (h + 1e-7)
                        f' (hw, hb) (dw, db) = (f hw dw, f hb db)
              newH@(newH1, newH2) = (f h1 dp1, f h2 dp2)
                  where f (hw, hb) (dw, db) = (dw ** 2 + hw, db ** 2 + hb)
newAdaGrad lr = AdaGrad lr ((0, 0), (0, 0))

initTestNet :: forall wc il is os. (KnownNat wc, KnownNat il, KnownNat is, KnownNat os) => Double -> IO (MnistTwoLayerNet wc il is os)
initTestNet wis = result
    where initParams = do
              w1 <- randn'
              let b1 = konst 0
              w2 <- randn'
              let b2 = konst 0
              return (w1 * realToFrac wis, b1, w2 * realToFrac wis, b2)
          result = do
              (w1, b1, w2, b2) <- initParams
              let affine1 = affineLayer (w1, b1)
              let relu = reluLayer
              let affine2 = affineLayer (w2, b2)
              return $ MnistTwoLayerNet affine1 relu affine2 $ newAdaGrad 0.1

type LastLayer il hs os = X il hs -> T il os

type X il is = L il is
type Y il wc = L il wc
type T il os = Y il os
type W is wc = L is wc
type B wc = R wc
type NeuralParam is wc = (W is wc, B wc)
type Prediction il wc = Y il wc
type Loss il wc = Y il wc
type Delta il is = X il is 
type DX il is = X il is
type ForwardF il is wc = X il is -> Y il wc
type BackwardF il is wc = Y il wc -> X il is

v2m :: forall m n. (KnownNat m, KnownNat n) => R n -> L m n
v2m v = fromList $ (>>= id) $ replicate mVal $ LA.toList $ extract v
    where mVal = fromIntegral $ natVal (Proxy :: Proxy m)

softmax :: forall m n. (KnownNat m, KnownNat n) => L m n -> L m n
softmax a = expA / sumExpA
    where maxs = foldAndExtractRow' LA.maxElement a
          a' = a - maxs
          expA = exp a'
          sumExpA = foldAndExtractRow' LA.sumElements expA

crossEntropyError :: forall m n. (KnownNat m, KnownNat n) => L m n -> L m n -> L m 1
crossEntropyError t y = negate $ foldRow' LA.sumElements $ t * log (y + delta)
    where delta = 1e-7

foldRow' :: forall m n. (KnownNat m, KnownNat n) => (LA.Vector Double -> Double) -> L m n -> L m 1
foldRow' f m = fromList $ map (f . extract) $ toRows m

foldAndExtractRow' :: forall m n. (KnownNat m, KnownNat n) => (LA.Vector Double -> Double) -> L m n -> L m n
foldAndExtractRow' f m = fromList $ toRows m >>= (replicate nVal . f . extract)
    where nVal = fromIntegral $ natVal (Proxy :: Proxy n)

randn' :: forall m n. (KnownNat m, KnownNat n) => IO (L m n)
randn' = result
    where mVal = fromIntegral $ natVal (Proxy :: Proxy m)
          nVal = fromIntegral $ natVal (Proxy :: Proxy n)
          result = do
              m <- LA.randn mVal nVal
              return $ case create m of
                  Nothing -> error "Something wrong"
                  Just x -> x

dmmap' :: forall m n. (KnownNat m, KnownNat n) => (Double -> Double) -> L m n -> L m n
dmmap' f m = mapped
    where m' = extract m
          mapped = case create $ LA.cmap f m' of
              Nothing -> error "Something wrong"
              Just x -> x

zipWithM' :: forall m n. (KnownNat m, KnownNat n) => (Double -> Double -> Double) -> L m n -> L m n -> L m n
zipWithM' f m1 m2 = result
    where m1' = extract m1
          m2' = extract m2
          result = build (\ r c -> f (atIndex m1' r c) (atIndex m2' r c))
              where atIndex m r c = LA.atIndex m (truncate r, truncate c)

maxIndex' :: forall m n. (KnownNat m, KnownNat n) => L m n -> L m 1
maxIndex' = foldRow' (fromIntegral . LA.maxIndex)
