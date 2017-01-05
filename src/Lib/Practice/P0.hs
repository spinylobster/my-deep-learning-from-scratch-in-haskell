module Lib.Practice.P0
    (
        f0
    ) where

import Numeric.LinearAlgebra
import Lib.Utility

f0 = do
    -- 配列の生成
    let x = vector [1..3]
    print x

    -- 加減乗除
    let y = vector [2, 4..6]
    print $ x + y
    print $ x - y
    print $ x * y
    print $ x / y
    
    -- ブロードキャスト
    print $ x / 2

    -- 行列
    let mA = (2><2) [1..4] :: Matrix Double
    print mA
    print $ size mA
    let mB = (2><2) [3, 0, 0, 6]
    print $ mA + mB
    print $ mA * mB

    -- 行列のブロードキャスト
    print $ mA * 10
    print $ mA * row [10, 20]
    -- ↑MatrixとVectorのelement-wiseな計算用の演算子がない？

    -- 要素へのアクセス
    let mX = (3><2) [51, 55, 14, 19, 0, 4 :: Double]
    print $ mX ! 0
    print $ mX ! 0 ! 1
    mapM_ print $ toRows mX
    let flattened = flatten mX
    print $ map (flattened !) $ [0, 2, 4]
    print $ map (==1) $ toList $ flattened .>. 15
    print $ filter (>15) $ toList flattened
