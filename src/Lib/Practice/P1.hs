module Lib.Practice.P1
    (
        f1
    ) where

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
    (sinStyle, sin) :
    (cosStyle, cos) :
    []
    where
        sinStyle = LineSpec.title "sin" $
            LineSpec.deflt
        cosStyle = LineSpec.title "cos" $
            LineSpec.deflt
        xs = linearScale 100 (0::Float, 6)

f1 :: IO Exception.ExitCode
f1 = GP.plotDefault sincos
