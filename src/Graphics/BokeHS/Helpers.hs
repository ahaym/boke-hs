{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

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
import Graphics.BokeHS.CDS

import Data.Foldable
import Data.Colour.Names
import GHC.TypeLits

-- | Flipped ($). Same as (&) from @Control.Lens@
(%>) :: a -> (a -> b) -> b
x %> f = f x
{-# INLINE (%>) #-}
infixl 1 %>

--sample glyph adder function
--r is a row type from which the data can be extracted
addLine :: 
    ( Foldable t
    , HasColumn r f1 BNum
    , HasColumn r f2 BNum
    , KnownSymbol f1
    , KnownSymbol f2
    )
    => t r -> Key f1 -> Key f2 -> LineConfig -> Plot -> Plot
addLine points k1 k2 config plt@Plot{renderers = rends} = 
    plt{renderers = lrend : rends} 
        where
            lrend = GRend GlyphRenderer { hoverGlyph = Nothing, mutedGlyph = Nothing,
                dataSource = src, glyph = lin, vie = CDSView}
            lin = Line config k1 k2
            src = CDS {
                rows = toList points,
                selected = Selection,
                selectionPolicy = UnionRenderers
            }

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
