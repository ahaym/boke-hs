{-# LANGUAGE OverloadedStrings #-}

module Graphics.BokeHS.Helpers(
    addLine
    ) where

import Graphics.BokeHS.Models
import Data.Scientific
import Data.Foldable

--sample glyph adder function
addLine :: Foldable t => Plot -> t r -> (r -> Scientific) -> (r -> Scientific) -> Color -> Plot
addLine plt@Plot{renderers = rends} points getx gety clr = 
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
