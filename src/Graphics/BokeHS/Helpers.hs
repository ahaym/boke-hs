{-# LANGUAGE OverloadedStrings #-}

module Graphics.BokeHS.Helpers(
    (%>),
    addLine,
    addLinearAxis,
    defaultToolbar,
    defaultPlot
    ) where

import Graphics.BokeHS.Models
import Graphics.BokeHS.Prim
import Graphics.BokeHS.GlyphConfig

import Data.Foldable
import Data.Colour.Names

-- | Flipped ($). Same as (&) from @Control.Lens@
(%>) :: a -> (a -> b) -> b
x %> f = f x
{-# INLINE (%>) #-}
infixl 1 %>

--sample glyph adder function
--r is a row type from which the data can be extracted
addLine :: Foldable t => t r -> (r -> BNum) -> (r -> BNum) -> LineConfig -> Plot -> Plot
addLine points getx gety config plt@Plot{renderers = rends} = 
    plt{renderers = lrend : rends} 
        where
            lrend = GRend GlyphRenderer { hoverGlyph = Nothing, mutedGlyph = Nothing,
                dataSource = src, glyph = lin, vie = CDSView}
            lin = Line config (Field "x") (Field "y")
            src = CDS {
                cols = [(Field "x", xs), (Field "y", ys)],
                selected = Selection,
                selectionPolicy = UnionRenderers
            }
            xs = getx <$> foldr' (:) [] points
            ys = gety <$> foldr' (:) [] points

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
