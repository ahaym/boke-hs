{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.BokeHS.Models where

import Data.Text (Text)
import GHC.Generics
import Data.Aeson
import Data.String (IsString)

import Graphics.BokeHS.Prim
import Graphics.BokeHS.GlyphConfig

newtype Placeholder = Placeholder Value deriving (Show, Generic, Eq)
instance ToJSON Placeholder

data Plot = Plot {
    backgroundFill :: Color,
    width :: BNum,
    height :: BNum,
    renderers :: [Renderer],
    title :: Title,
    toolbar :: Toolbar,
    xRange :: Range,
    yRange :: Range,
    xScale :: Scale,
    yScale :: Scale
    } deriving Show

newtype Title = Title Text deriving (Show, IsString)

data Renderer = ARend Direction Axis | GRend GlyphRenderer deriving Show

data Axis = LinearAxis {
  formatter :: Formatter
  , ticker :: Ticker
  } deriving Show

data DataSource = forall v. ToJSON v => CDS {
  cols :: [(Field, [v])]  -- FIXME use `Frames` instead
  , selected :: Selection
  , selectionPolicy :: SelectionPolicy
  }

instance Show DataSource where
    show CDS{} = "<CDS>"

data GlyphRenderer = GlyphRenderer {
  hoverGlyph :: Maybe Placeholder
  , mutedGlyph :: Maybe Placeholder
  , dataSource :: DataSource
  , glyph :: Glyph
  , vie :: View } deriving Show

data View = CDSView | Views_ deriving Show
data ViewWrapper = VWrap Value View

data Scale = LinearScale deriving Show

data Ticker = BasicTicker deriving Show
 
data Formatter = BasicTickFormatter deriving Show

data Range = Range1d {
        start :: BNum,
        end :: BNum } deriving Show

data SelectionPolicy = UnionRenderers | Policies_ deriving Show

data Selection = Selection | Sels_ deriving Show

data Glyph = Line {
        lineConfig :: LineConfig
        , xfield :: Field
        , yfield :: Field } 
            deriving Show

data Auto a = Auto | NotAuto a deriving Show

--active_drag, active_inspect, active_scroll, active_tap 
data Toolbar = Toolbar {    
        activeDrag :: Auto Placeholder
        , activeInspect :: Auto Placeholder
        , activeScroll :: Auto Placeholder
        , activeTap :: Auto Placeholder } deriving Show
