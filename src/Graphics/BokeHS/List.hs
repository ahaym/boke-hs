{-# LANGUAGE OverloadedStrings #-}

module Graphics.BokeHS.List where

import Graphics.BokeHS.Models
import Data.Scientific

--sample glyph adder function
addLine :: Plot -> [(Scientific, Scientific)] -> Color -> Plot
addLine plt@Plot{renderers = rends} points clr = plt{renderers = lrend : rends} 
    where
        lrend = GRend GlyphRenderer { hoverGlyph = Nothing, mutedGlyph = Nothing,
            dataSource = src, glyph = lin, vie = CDSView}
        lin = Line clr (Field "x") (Field "y")
        src = CDS {
                cols = [(Field "x", xs), (Field "y", ys)],
                selected = Selection,
                selectionPolicy = UnionRenderers
            }
        xs = fst <$> points
        ys = snd <$> points
