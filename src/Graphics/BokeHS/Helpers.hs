{-# LANGUAGE OverloadedStrings #-}

module Graphics.BokeHS.Helpers(
    (|>),
    ($>),
    addLine,
    addMultiLine,
    addLinearAxis,
    defaultToolbar,
    defaultPlot
    ) where

import Graphics.BokeHS.Models
import Data.Foldable
import Data.Colour.Names

(|>) :: (a -> b) -> (b -> c) -> a -> c
(|>) = flip (.)

($>) :: a -> (a -> b) -> b
($>) = flip ($)
infixr 0 $>

--sample glyph adder function
--r is a row type from which the data can be extracted
addLine :: Foldable t => t r -> (r -> BNum) -> (r -> BNum) -> Color -> Plot -> Plot
addLine points getx gety clr plt@Plot{renderers = rends} = 
    plt{renderers = lrend : rends} 
        where
            lrend = GRend GlyphRenderer { hoverGlyph = Nothing, mutedGlyph = Nothing,
                dataSource = src, glyph = lin, vie = CDSView}
            lin = Line clr (Field "x") (Field "y")
            src = CDS {
                cols = [(Field "x", NumSpec xs), (Field "y", NumSpec ys)],
                selected = Selection,
                selectionPolicy = UnionRenderers
            }
            xs = getx <$> foldr' (:) [] points
            ys = gety <$> foldr' (:) [] points

--adds multiple lines of the same color.
addMultiLine :: (Foldable t0, Foldable t1) 
    => t1 (t0 r) --singly nested structure of data, like [[r]]
    -> (r -> BNum) --x getter
    -> (r -> BNum) --y getter
    -> Color --line color
    -> Plot --plot to add lines to 
    -> Plot --plot with lines added
addMultiLine lns getx gety clr plt = foldr' ($) plt fs
    where 
        fs = go <$> foldr' (:) [] lns
        go ps = addLine ps getx gety clr
        
--sample layout adder function
addLinearAxis :: Direction -> Plot -> Plot
addLinearAxis dir plt@Plot{renderers = rends} = 
    plt{renderers = arend : rends}
        where
            ax = LinearAxis{formatter=BasicTickFormatter, ticker=BasicTicker}
            arend = ARend dir ax


defaultToolbar :: Toolbar
defaultToolbar = Toolbar Auto Auto Auto Auto

defaultPlot :: Plot
defaultPlot = Plot {
       backgroundFill = white,
       width = 0,
       height = 0,
       renderers = [],
       title = Title "Default BokeHS plot",
       toolbar = defaultToolbar,
       xRange = Range1d 0 0,
       yRange = Range1d 0 0,
       xScale = LinearScale,
       yScale = LinearScale
    } 
