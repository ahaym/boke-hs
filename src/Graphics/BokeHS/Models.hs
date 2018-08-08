{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Graphics.BokeHS.Models where

import Data.Text (Text)
import GHC.Generics
import GHC.TypeLits
import Data.Aeson
import Data.String (IsString)

import Graphics.BokeHS.Prim
import Graphics.BokeHS.GlyphConfig
import Graphics.BokeHS.CDS

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
    } 

newtype Title = Title Text deriving IsString

data Renderer 
    = ARend Direction Axis 
    | forall r. GRend (GlyphRenderer r)

data Axis = LinearAxis {
  formatter :: Formatter
  , ticker :: Ticker
  } 

data DataSource r = CDS {
  rows :: [r]  -- FIXME use `Frames` instead
  , selected :: Selection
  , selectionPolicy :: SelectionPolicy
  } 

data GlyphRenderer r = GlyphRenderer {
  hoverGlyph :: Maybe Placeholder
  , mutedGlyph :: Maybe Placeholder
  , dataSource :: DataSource r
  , glyph :: Glyph r
  , vie :: View } 

data View = CDSView | Views_ 
data ViewWrapper = VWrap Value View 

data Scale = LinearScale 

data Ticker = BasicTicker 
 
data Formatter = BasicTickFormatter 

data Range = Range1d {
        start :: BNum,
        end :: BNum } 

data SelectionPolicy = UnionRenderers | Policies_ 

data Selection = Selection | Sels_ 

data Glyph r where
    Line :: (KnownSymbol n0, HasColumn r n0 BNum, KnownSymbol n1, HasColumn r n1 BNum) =>
        LineConfig -> Key n0 -> Key n1 -> Glyph r

getNames :: Glyph r -> [Name r]
getNames (Line _ x y) = [Name x, Name y]

data Auto a = Auto | NotAuto a 

--active_drag, active_inspect, active_scroll, active_tap 
data Toolbar = Toolbar {    
        activeDrag :: Auto Placeholder
        , activeInspect :: Auto Placeholder
        , activeScroll :: Auto Placeholder
        , activeTap :: Auto Placeholder } 
