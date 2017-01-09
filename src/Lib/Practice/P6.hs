module Lib.Practice.P6
    (
        f6
    ) where

import Lib.Utility.Mnist
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Numeric.LinearAlgebra.Static
import qualified Numeric.LinearAlgebra as LA

f6' :: MaybeT IO ()
f6' = do
    ((trD, trL), _) <- getMnistData
    MaybeT $ fmap Just $ print $ size trD
    let (l, d) = (extract trL LA.! 0, extract trD LA.! 0)
    liftIO $ print d
    liftIO $ print l
    -- TODO 画像の表示

f6 :: IO ()
f6 = do
    let (MaybeT result) = f6'
    result' <- result
    case result' of
        Nothing -> print "失敗"
        Just x -> return x
