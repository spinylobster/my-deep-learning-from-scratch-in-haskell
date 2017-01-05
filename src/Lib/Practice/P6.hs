module Lib.Practice.P6
    (
        f6
    ) where

import Lib.Utility
import Control.Monad.Trans.Maybe
import Numeric.LinearAlgebra

f6' :: MaybeT IO ()
f6' = do
    ((trD, trL), _) <- getMnistData
    MaybeT $ fmap Just $ print $ size trD
    let (l, d) = (trL ! 0, trD ! 0)
    MaybeT $ fmap Just $ print d
    MaybeT $ fmap Just $ print l
    -- TODO 画像の表示

f6 :: IO ()
f6 = result
    where (MaybeT m)    = f6'
          result        = do
            result' <- m
            case result' of
                Nothing -> return ()
                Just x -> return x
