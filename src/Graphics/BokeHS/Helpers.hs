{-# LANGUAGE OverloadedStrings #-}

module Graphics.BokeHS.Helpers(
    (|>),
    ($>),
    addLine,
    addLinearAxis,
    defaultToolbar,
    defaultPlot
    ) where

import Graphics.BokeHS.Models
import Data.Scientific
import Data.Foldable
import Data.Colour.Names

(|>) :: (a -> b) -> (b -> c) -> a -> c
(|>) = flip (.)
infixr 0 $>

($>) :: a -> (a -> b) -> b
($>) = flip ($)

--sample glyph adder function
addLine :: Foldable t => t r -> (r -> Scientific) -> (r -> Scientific) -> Color -> Plot -> Plot
addLine points getx gety clr plt@Plot{renderers = rends} = 
    plt{renderers = lrend : rends} 
        where
            lrend = GRend GlyphRenderer { hoverGlyph = Nothing, mutedGlyph = Nothing,
                dataSource = src, glyph = lin, vie = CDSView}
            lin = Line clr (Field "x") (Field "y")
            src = CDS {
                cols = [(Field "x", xs), (Field "y", ys)],
                selected = Selection,
                selectionPolicy = UnionRenderers
            }
            xs = getx <$> foldr' (:) [] points
            ys = gety <$> foldr' (:) [] points

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
