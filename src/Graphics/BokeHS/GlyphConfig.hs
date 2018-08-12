{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Configuration types for every Glyph (everything that's not a @Spec@)
module Graphics.BokeHS.GlyphConfig where

import Graphics.BokeHS.Prim

import Data.Aeson
import Data.Default
import Data.Colour.Names
import Data.Text

class GlyphConfig a where
    mkConfig :: a -> [(Text, Value)]

data LineConfig = LineConfig
    { lineColor :: Color
    , lineAlpha :: BNum
    , lineCap :: LineCap
    , lineDash :: DashPattern
    , lineDashOffset :: Int
    , lineJoin :: LineJoin
    , lineWidth :: BNum
        } deriving (Show)

instance Default LineConfig where
    def = LineConfig 
        { lineColor = black
        , lineAlpha = 1.0
        , lineCap = Butt
        , lineDash = Solid
        , lineDashOffset = 0
        , lineJoin = Miter
        , lineWidth = 1.0
        }

instance GlyphConfig LineConfig where
    mkConfig LineConfig{..} =
        [ ("line_color", c2j lineColor)
        , ("line_alpha", toJSON lineAlpha)
        , ("line_cap", toJSON lineCap)
        , ("line_dash", toJSON lineDash)
        , ("line_dash_offset", toJSON lineDashOffset)
        , ("line_join", toJSON lineJoin)
        , ("line_width", toJSON lineWidth)
        ]
