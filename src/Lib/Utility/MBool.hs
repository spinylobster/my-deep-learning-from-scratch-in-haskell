{-# LANGUAGE FlexibleContexts #-}

module Lib.Utility.MBool where

import Numeric.LinearAlgebra

infix  4  .==., ./=., .<., .<=., .>=., .>.
infixr 3  .&&.
infixr 2  .||.

-- specialized for Int result
cond'
    :: (Element t, Ord t, Container c I, Container c t)
    => c t -> c t -> c I -> c I -> c I -> c I
cond' = cond

a .<.  b = cond' a b 1 0 0
a .<=. b = cond' a b 1 1 0
a .==. b = cond' a b 0 1 0
a ./=. b = cond' a b 1 0 1
a .>=. b = cond' a b 0 1 1
a .>.  b = cond' a b 0 0 1

a .&&. b  = step (a*b)
a .||. b  = step (a+b)
no a      = 1-a
xor a b   = a ./=. b
equiv a b = a .==. b
imp a b   = no a .||. b

taut x = minElement x == 1

minEvery a b = cond a b a a b
maxEvery a b = cond a b b b a
