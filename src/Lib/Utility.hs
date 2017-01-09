{-# LANGUAGE FlexibleContexts #-}

module Lib.Utility 
(
    downMaybeTIO,
    module A,
    module B,
    module C,
    module D
) where

import Lib.Utility.MBool as A
import Lib.Utility.Mnist as B
import Lib.Utility.Differentiation as C
import Lib.Utility.NeuralNet as D
import Control.Monad.Trans.Maybe

downMaybeTIO :: MaybeT IO () -> IO ()
downMaybeTIO action = do
    let (MaybeT result) = action
    result' <- result
    case result' of
        Nothing -> print "失敗"
        Just x -> return x
