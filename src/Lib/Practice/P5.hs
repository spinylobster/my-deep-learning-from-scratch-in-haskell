module Lib.Practice.P5
    (
        f5
    ) where

import Numeric.LinearAlgebra
import Lib.Utility

f5 = do
    let sigmoid x = 1 / (1 + exp(-x))
    let calcA x w b = add b $ x <> w
    let x = (1><2) [1,0.5 :: Double]
    let w1 = (2><3) [0.1,0.3,0.5,0.2,0.4,0.6]
    let b1 = (1><3) [0.1,0.2,0.3]
    let a1 = calcA x w1 b1
    let z1 = cmap sigmoid a1
    print z1

    let w2 = (3><2) [0.1,0.4,0.2,0.5,0.3,0.6]
    let b2 = (1><2) [0.1,0.2]
    let a2 = calcA z1 w2 b2
    let z2 = cmap sigmoid a2
    print z2

    let w3 = (2><2) [0.1,0.3,0.2,0.4]
    let b3 = (1><2) [0.1,0.2]
    let a3 = calcA z2 w3 b3
    let y = a3
    print y

    let a4 = (1><3) [1010,1000,900 :: Double]
    print $ sumElements $ softmax a4
