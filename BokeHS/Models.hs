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

l2o :: [(Text, Value)] -> Value
l2o = Object . fromList

--encodes a BokehJS Ref ID
newtype BID = BID Text deriving (Eq, Show, Generic)
instance ToJSON BID

--encodes a BokehJS Type Declaration
newtype BType = BType Text deriving (Eq, Show, Generic)
instance ToJSON BType

newtype BNode = BNode Value deriving (Show, Generic)
instance ToJSON BNode 

--holds serialization state
data SerialEnv = SerialEnv {
        nodes :: [BNode], --list of nodes in the graph
        idCount :: Int --current BID generation state 
    } deriving Show

{- Class for types that can be serialized to BokehJS.
 - Can either be a primitive type whose value can be directly inserted (makePrim).
 - Or a ref, trivially constructed from the typename, is inserted in place of the
 - value, and the value itself is added to the node list of the BokehJS graph.
 -}

class Bokeh a where
    makePrim :: a -> Value
    serializeNode :: a -> State SerialEnv Value
    makePrim v = Null
    serializeNode o = return $ makePrim o
    {-# MINIMAL makePrim | serializeNode #-}

instance Bokeh Value where
    makePrim = id

--Helper function for getting new BIDs
newBID :: State SerialEnv BID
newBID = do
    env <- get
    let curID = idCount env
    put env{idCount = curID + 1}
    return $ (BID . pack . show) curID

--gets the current ID
--probably should use an MTL-style typeclass for this stuff
--to ensure no modifications
curID :: State SerialEnv BID
curID = (BID . pack . show . idCount) <$> get

--Helper function for adding nodes to the node list
addNode :: BNode -> State SerialEnv ()
addNode newVal = do
    env <- get
    let vals = nodes env
    put env{nodes=newVal : vals}

--helper function for conveniently turning a list of attributes into a seralizing function
type Attributes = [(Text, Value)]
makeRef :: BType -> Attributes -> State SerialEnv Value
makeRef btype attrs = do
    curID <- newBID
    let footer = l2o [("id", toJSON curID), ("type", toJSON btype)]
        attrObj = l2o [("attributes", (Object . fromList) attrs)]
        node = (BNode . mergeAeson) [footer, attrObj]
    addNode node
    return footer
    where 
        mergeAeson :: [Value] -> Value
        mergeAeson = Object . HML.unions . map (\(Object x) -> x)

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

instance Bokeh Plot where
    serializeNode plt = do
        title_ <- serializeNode (title plt)
        let plotObj = [("title", title_)]
        makeRef (BType "Plot") plotObj
       

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
    serializeNode (Title titletext) = makeRef (BType "Title") titleObj
            where
                 titleObj = [
                    ("attributes", Object $ fromList [
                        ("plot", Null), ("text", String titletext)])
                    ]
data Direction = BLeft | BRight | BAbove | BBelow | BCenter deriving (Eq, Show)

data Renderer = ARend Direction Axis | GRend GlyphRenderer deriving Show

newtype Field = Field Text deriving (Show, Generic)
instance ToJSON Field

data Axis = LinearAxis {
        formatter :: Formatter,
        ticker :: Ticker
    } deriving Show

data AxisWrapper = AxWrap Value Axis

instance Bokeh AxisWrapper where
    --TODO: Validate Parent Value
    serializeNode  (AxWrap parentRef (LinearAxis form tick)) = do
        formatter_ <- serializeNode form 
        ticker_ <- serializeNode tick
        let axisObj = [("formatter", formatter_), ("plot", parentRef), ("ticker", ticker_)]
        makeRef (BType "LinearAxis") axisObj

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

data View = CDSView | Views_ deriving Show
data ViewWrapper = VWrap Value View

instance Bokeh ViewWrapper where
    serializeNode (VWrap cdsRef CDSView) = makeRef (BType "CDSView") viewObj
        where viewObj = [("source", cdsRef)]

data Scale = LinearScale deriving Show

instance Bokeh Scale where
    serializeNode LinearScale = makeRef (BType "LinearScale") []

data Ticker = BasicTicker deriving Show
 
instance Bokeh Ticker where
    serializeNode BasicTicker = makeRef (BType "BasicTicker") []


data Formatter = BasicTickFormatter deriving Show

instance Bokeh Formatter where
    serializeNode BasicTickFormatter = makeRef (BType "BasicTickFormatter") []


data Range = Range1d {
        callback :: Maybe (),
        start :: Scientific,
        end :: Scientific
    } deriving Show

data SelectionPolicy = UnionRenderers | Policies_ deriving Show

instance Bokeh SelectionPolicy where
    serializeNode UnionRenderers = makeRef (BType "UnionRenderers") []

data Selection = Selection | Sels_ deriving Show

instance Bokeh Selection where
    serializeNode Selection = makeRef (BType "UnionRenderers") []

data Glyph = Line {
        lineColor :: Color,
        x :: Field,
        y :: Field
    } deriving Show

instance Bokeh Glyph where
    serializeNode (Line color x y) = makeRef (BType "Line") [("line_color", makePrim color), 
        ("x", l2o [("field", toJSON x)]),("y", l2o [("field", toJSON y)])]

data Auto a = Auto | NotAuto a deriving Show
instance (Bokeh a) => Bokeh (Auto a) where
    serializeNode Auto = return $ String "auto"
    serializeNode (NotAuto o) = serializeNode o

--active_drag, active_inspect, active_scroll, active_tap 
data Toolbar = Toolbar {    
        activeDrag :: Auto Value,
        activeInspect :: Auto Value,
        activeScroll :: Auto Value,
        activeTap :: Auto Value
    }
    deriving Show

defaultToolbar :: Toolbar
defaultToolbar = Toolbar Auto Auto Auto Auto

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
       toolbar = defaultToolbar,
       xRange = Range1d Nothing (-0.5) 20,
       yRange = Range1d Nothing (-0.5) 20,
       xScale = LinearScale,
       yScale = LinearScale
    } where
        xaxis = ARend BBelow ax
        yaxis = ARend BAbove ax
        ax = LinearAxis{formatter=BasicTickFormatter, ticker=BasicTicker}
        lrend = GRend GlyphRenderer { dataSource = samplesrc,
            glyph = lin, vie = CDSView}
        lin = Line Lavender (Field "x") (Field "y")
