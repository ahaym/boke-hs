{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AutoDeriveTypeable #-}

module BokeHS.Models where

import Data.Word
import Data.Text (pack, Text)
import Data.Scientific
import GHC.Generics
import GHC.Exts (fromList)
import Control.Monad.State
import Data.Aeson
import qualified Data.HashMap.Lazy as HML

--encodes a BokehJS Ref ID
newtype BID = BID Text deriving (Eq, Show, Generic)
instance ToJSON BID

--encodes a BokehJS Type Declaration
newtype BType = BType Text deriving (Eq, Show, Generic)
instance ToJSON BType

{- Class for types that can be serialized to BokehJS.
 - Can either be a primitive type whose value can be directly inserted (BPrim).
 - Or a ref; the BRef (Constructed from the typename)is inserted in place of the
 - value, and the value itself is added to the reference list of the BokehJS graph.
 -}

type SerialEnv = [Value]
class Bokeh a where
    makePrim :: a -> Value
    serializeNode :: BID -> a -> State SerialEnv Value
    makePrim v = Null
    serializeNode _ o = return $ makePrim o
    {-# MINIMAL makePrim | serializeNode #-}

makeRef :: (BID -> a -> (Text, Value)) -> BID -> a -> State SerialEnv Value
makeRef f obj bid = return $ merge_aeson [footer, body]
    where
        (btype, body) = f obj bid
        merge_aeson :: [Value] -> Value
        merge_aeson = Object . HML.unions . map (\(Object x) -> x)
        footer = Object $ fromList [("id", String "foo-bar3"), ("type", String btype)]


data Plot = Plot {
    backgroundFill :: Color,
    width :: Word32,
    height :: Word32,
    renderers :: [Renderer],
    title :: Title,
    toolbar :: Toolbar,
    xRange :: Range,
    yRange :: Range,
    xScale :: Scale,
    yScale :: Scale
    } deriving Show

data Color = Purple | White | Lavender deriving Show

instance Bokeh Color where
    makePrim color = toJSON colorString
        where
            colorString :: Text
            colorString = case color of
                Purple -> "purple"
                White -> "white"
                Lavender -> "lavender"

newtype Title = Title Text deriving Show
instance Bokeh Title where
    serializeNode = makeRef makeNode where
        makeNode (BID idtxt) (Title titletext)  = ("Title", titleObj)
            where
                 titleObj = Object $ fromList [
                    ("attributes", Object $ fromList [
                        ("plot", Null), ("text", String titletext)]),
                    ("id", String idtxt),
                    ("type", String "Title")
                    ]
data Direction = BLeft | BRight | BAbove | BBelow | BCenter deriving (Eq, Show)

data Renderer = ARend Direction Axis | GRend GlyphRenderer deriving Show

newtype Field = Field Text deriving Show

data Axis = LinearAxis {
        formatter :: Formatter,
        ticker :: Ticker
    } deriving Show

data DataSource = CDS {
        cols :: [(Field, [Scientific])],
        selected :: Selection,
        selectionPolicy :: SelectionPolicy
    } deriving Show

data GlyphRenderer = GlyphRenderer {
        --hover, muted are auto
        dataSource :: DataSource,
        glyph :: Glyph,
        vie :: View -- possibly not needed
    } deriving Show

data View = CDSView DataSource | Views_ deriving Show

data Scale = LinearScale | Scales_ deriving Show

data Ticker = BasicTicker | Ticker_ deriving Show

data Formatter = BasicTickFormatter deriving Show

data Range = Range1d {
        start :: Scientific,
        end :: Scientific
    } deriving Show

data SelectionPolicy = UnionRenderers | Policies_ deriving Show

data Selection = Selection | Sels_ deriving Show

data Glyph = Line {
        lineColor :: Color,
        x :: Field,
        y :: Field
    } deriving Show

data Toolbar = ToolBar --active_drag, active_inspect, active_scroll, active_tap 
    deriving Show

samplesrc :: DataSource
samplesrc = CDS {
        cols = [(Field "x", xcols), (Field "y", ycols)],
        selected = Selection,
        selectionPolicy = UnionRenderers
    } where xcols = [-0.5,
                     1.8333333333333335,
                     4.166666666666667,
                     6.5,
                     8.833333333333334,
                     11.166666666666668,
                     13.5,
                     15.833333333333336,
                     18.166666666666668,
                     20.5]
            ycols = [2.75,
                     3.916666666666667,
                     5.083333333333334,
                     6.25,
                     7.416666666666667,
                     8.583333333333334,
                     9.75,
                     10.916666666666668,
                     12.083333333333334,
                     13.25] 

examplePlot :: Plot
examplePlot = Plot{
       backgroundFill = Lavender,
       width = 400,
       height = 400,
       renderers = [xaxis, yaxis, lrend],
       title = Title "Sample Haskell Plot",
       toolbar = ToolBar,
       xRange = Range1d (-0.5) 20,
       yRange = Range1d (-0.5) 20,
       xScale = LinearScale,
       yScale = LinearScale
    } where
        xaxis = ARend BBelow ax
        yaxis = ARend BAbove ax
        ax = LinearAxis{formatter=BasicTickFormatter, ticker=BasicTicker}
        lrend = GRend GlyphRenderer { dataSource = samplesrc,
            glyph = lin, vie = CDSView samplesrc}
        lin = Line Lavender (Field "x") (Field "y")
