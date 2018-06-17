{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.BokeHS.Models where

import Data.Text (Text)
import Data.Scientific
import GHC.Generics
import Data.Aeson
import Data.String (IsString)
import qualified Data.Colour as C
import qualified Data.Colour.SRGB as C

--encodes a BokehJS Ref ID
newtype BID = BID Text deriving (Eq, Show, Generic)
instance ToJSON BID

--encodes a BokehJS Type Declaration
newtype BType = BType Text deriving (Eq, Show, Generic)
instance ToJSON BType

newtype BNode = BNode Value deriving (Show, Generic)
instance ToJSON BNode 

newtype Placeholder = Placeholder Value deriving (Show, Generic, Eq)
instance ToJSON Placeholder

data Plot = Plot {
    backgroundFill :: C.Colour Double,
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

type Color = C.Colour Double

type BNum = Scientific

--Represents a floating-point angle in degrees
newtype Angle = Angle BNum deriving (Show, Generic, Eq, Num)
instance ToJSON Angle

newtype Title = Title Text deriving (Show, IsString)

data Direction = BLeft | BRight | BAbove | BBelow | BCenter deriving (Eq, Show)

data Renderer = ARend Direction Axis | GRend GlyphRenderer deriving Show

newtype Field = Field Text deriving (Show, Generic)
instance ToJSON Field

data Axis = LinearAxis {
  formatter :: Formatter
  , ticker :: Ticker
  } deriving Show


data Spec = 
     NumSpec [BNum]
   | ColorSpec [Color]
   deriving Show
instance ToJSON Spec where
    toJSON (NumSpec xs) = toJSON xs
    toJSON (ColorSpec xs) = toJSON $ C.sRGB24show <$> xs

data DataSource = CDS {
  cols :: [(Field, Spec)]  -- FIXME use `Frames` instead
  , selected :: Selection
  , selectionPolicy :: SelectionPolicy
  } deriving Show


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
        lineColor :: Color
        , xfield :: Field
        , yfield :: Field } deriving Show

data Auto a = Auto | NotAuto a deriving Show

--active_drag, active_inspect, active_scroll, active_tap 
data Toolbar = Toolbar {    
        activeDrag :: Auto Placeholder
        , activeInspect :: Auto Placeholder
        , activeScroll :: Auto Placeholder
        , activeTap :: Auto Placeholder } deriving Show
