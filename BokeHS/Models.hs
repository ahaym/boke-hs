{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AutoDeriveTypeable #-}

module BokeHS.Models where

import Data.Text (pack, Text)
import Data.Scientific
import GHC.Generics
import GHC.Exts (fromList)
import Control.Monad.State
import Data.Aeson
import qualified Data.HashMap.Lazy as HML
import qualified Data.ByteString.Lazy as BS
import Data.Aeson.Encode.Pretty (encodePretty)

bokehVersion :: Text
bokehVersion = "0.12.16"

mergeAeson :: [Value] -> Value
mergeAeson = Object . HML.unions . map (\(Object o) -> o)

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
    makePrim _ = Null
    serializeNode o = return $ makePrim o
    {-# MINIMAL makePrim | serializeNode #-}

newtype Placeholder = Placeholder Value deriving (Show, Generic, Eq)
instance ToJSON Placeholder

instance Bokeh Placeholder where
    makePrim (Placeholder v)= v

instance (Bokeh a) => Bokeh (Maybe a) where
    serializeNode Nothing = return Null
    serializeNode (Just o) = serializeNode o

--Helper function for getting new BIDs
newBID :: State SerialEnv BID
newBID = do
    env <- get
    let curID = idCount env
    put env{idCount = curID + 1}
    return $ (BID . pack . show) curID

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

data Plot = Plot {
    backgroundFill :: Color,
    width :: Scientific,
    height :: Scientific,
    renderers :: [Renderer],
    title :: Title,
    toolbar :: Toolbar,
    xRange :: Range,
    yRange :: Range,
    xScale :: Scale,
    yScale :: Scale
    } deriving Show

serializePlot :: Plot -> State SerialEnv (BID, Value)
serializePlot plt@Plot{height = plot_height, width = plot_width} = do
        curID <- newBID
        let footer = l2o [(pack "id", toJSON curID), (pack "type", toJSON $ BType"Plot")]
        background_fill_ <- serializeNode (backgroundFill plt)
        title_ <- serializeNode (title plt)
        toolbar_ <- serializeNode (toolbar plt)
        x_range_ <- serializeNode (xRange plt)
        y_range_ <- serializeNode (yRange plt)
        x_scale_ <- serializeNode (xScale plt)
        y_scale_ <- serializeNode (yScale plt) 
        renderers_ <- mapM (serializeRend footer) (renderers plt)
        let lefts = (toJSON . map snd) $ filter (dPred BLeft) renderers_
            rights = (toJSON . map snd) $ filter (dPred BRight) renderers_
            aboves = (toJSON . map snd) $ filter (dPred BAbove) renderers_
            belows = (toJSON . map snd) $ filter (dPred BBelow) renderers_
            plotAttrs = l2o [
                ("background_fill_color", l2o [("value", background_fill_)]),
                ("title", title_),
                ("plot_height", toJSON plot_height),
                ("plot_width", toJSON plot_width),
                ("below", belows),
                ("above", aboves),
                ("right", rights),
                ("left",  lefts),
                ("renderers", (toJSON . map snd) renderers_),
                ("title", title_),
                ("toolbar", toolbar_),
                ("x_range", x_range_),
                ("y_range", y_range_),
                ("x_scale", x_scale_),
                ("y_scale", y_scale_)
                ]
            plotObj = l2o [("attributes", plotAttrs)]
            node = (BNode . mergeAeson) [footer, plotObj]
        addNode node
        return (curID,footer)
        where
            dPred _ (Nothing, _) = False
            dPred dir0 (Just dir1, _) = dir0 == dir1
            serializeRend :: Value -> Renderer -> State SerialEnv (Maybe Direction, Value)
            serializeRend parentRef (ARend dir ax) =
                (,) (Just dir) <$> serializeNode (AxWrap parentRef ax) 
            serializeRend _ (GRend v) = (,) Nothing <$> serializeNode v

instance Bokeh Plot where
    serializeNode plt = snd <$> serializePlot plt

makeBokeh :: Plot -> Value
makeBokeh plt = let
    (go, endState) = runState (serializePlot plt) (SerialEnv [] 0)
    (pid, _) = go
    topid = (pack . show . idCount) endState
    topObj = l2o [(topid, containerObj)]
    containerObj = l2o [
        ("roots", rootObj),
        ("title", String "BokeHS Application"),
        ("version", String bokehVersion)
        ]
    rootObj = l2o [
        ("references", toJSON $ nodes endState),
        ("root_ids", toJSON pid)
        ]
    in topObj

printVal :: Value -> IO ()
printVal = BS.putStr . encodePretty

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

instance Bokeh DataSource where
    serializeNode cds = do
        selected_ <- serializeNode (selected cds)
        selectionPolicy_ <- serializeNode (selectionPolicy cds)
        let cdsObj = [("callback", Null), ("data", dataObj), 
                ("selected", selected_), ("selection_policy", selectionPolicy_)]
        makeRef (BType "ColumnDataSource") cdsObj
        where toObj (Field ftext, nums) = (ftext, toJSON nums)
              dataObj = (Object . fromList) $ toObj <$> cols cds

data GlyphRenderer = GlyphRenderer {
        hoverGlyph :: Maybe Placeholder,
        mutedGlyph :: Maybe Placeholder,
        dataSource :: DataSource,
        glyph :: Glyph,
        vie :: View
    } deriving Show

instance Bokeh GlyphRenderer where
    serializeNode gr = do
        hover_glyph_ <- serializeNode (hoverGlyph gr)
        muted_glyph_ <- serializeNode (mutedGlyph gr)
        data_source_ <- serializeNode (dataSource gr)
        glyph_ <- serializeNode (glyph gr)
        view_ <- serializeNode (VWrap data_source_ (vie gr))
        let grObj = [("hover_glyph", hover_glyph_), ("muted_glyph", muted_glyph_),
                ("data_source", data_source_), ("glyph", glyph_), ("view", view_)]
        makeRef (BType "GlyphRenderer") grObj

data View = CDSView | Views_ deriving Show
data ViewWrapper = VWrap Value View

instance Bokeh ViewWrapper where
    serializeNode (VWrap cdsRef CDSView) = makeRef (BType "CDSView") viewObj
        where viewObj = [("source", cdsRef)]
    serializeNode _ = undefined

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
        start :: Scientific,
        end :: Scientific
    } deriving Show

instance Bokeh Range where
    serializeNode range = makeRef (BType "Range1d") 
        [("plot", Null), ("start", toJSON (start range)), ("end", toJSON (end range))]

data SelectionPolicy = UnionRenderers | Policies_ deriving Show

instance Bokeh SelectionPolicy where
    serializeNode UnionRenderers = makeRef (BType "UnionRenderers") []
    serializeNode _ = undefined

data Selection = Selection | Sels_ deriving Show

instance Bokeh Selection where
    serializeNode Selection = makeRef (BType "Selection") []
    serializeNode _ = undefined

data Glyph = Line {
        lineColor :: Color,
        xfield :: Field,
        yfield :: Field
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
        activeDrag :: Auto Placeholder,
        activeInspect :: Auto Placeholder,
        activeScroll :: Auto Placeholder,
        activeTap :: Auto Placeholder
    }
    deriving Show

--CUR
instance Bokeh Toolbar where
    serializeNode bar = do
        active_drag_ <- serializeNode (activeDrag bar)
        active_inspect_ <- serializeNode (activeInspect bar)
        active_scroll_ <- serializeNode (activeScroll bar)
        active_tap_ <- serializeNode (activeTap bar)
        let barObj = [("active_drag", active_drag_), ("active_scroll", active_scroll_),
                ("active_inspect", active_inspect_), ("active_tap", active_tap_)]
        makeRef (BType "Toolbar") barObj

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
       xRange = Range1d (-0.5) 20,
       yRange = Range1d (-0.5) 20,
       xScale = LinearScale,
       yScale = LinearScale
    } where
        xaxis = ARend BBelow ax
        yaxis = ARend BAbove ax
        ax = LinearAxis{formatter=BasicTickFormatter, ticker=BasicTicker}
        lrend = GRend GlyphRenderer { hoverGlyph = Nothing, mutedGlyph = Nothing,
        dataSource = samplesrc, glyph = lin, vie = CDSView}
        lin = Line Lavender (Field "x") (Field "y")
