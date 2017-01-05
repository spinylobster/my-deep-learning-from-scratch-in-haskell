module Lib.Practice.P4
    (
        f4
    ) where

import Control.Applicative
import Graphics.Gnuplot.Advanced as GP
import Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import Graphics.Gnuplot.LineSpecification as LineSpec
import Graphics.Gnuplot.Value.Atom as Atom
import GHC.IO.Exception as Exception

sincos :: Plot2D.T Float Float
sincos = foldMap (\(myLineStyle, f) ->
    fmap (Graph2D.lineSpec myLineStyle) $
        Plot2D.function Graph2D.lines xs f) $
    [
        (stepStyle, step),
        (sigmoidStyle, sigmoid),
        (reluSyule, relu)
    ]
    where
        stepStyle = LineSpec.title "Step" LineSpec.deflt
        step x = if x > 0 then 1 else 0
        sigmoidStyle = LineSpec.title "Sigmoid" LineSpec.deflt
        sigmoid x = 1 / (1 + exp(-x))
        reluSyule = LineSpec.title "Sigmoid" LineSpec.deflt
        relu x = max 0 x

        xs = linearScale 1000 (-6::Float, 6)

f4 :: IO Exception.ExitCode
f4 = GP.plotDefault sincos
