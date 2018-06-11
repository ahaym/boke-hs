{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AutoDeriveTypeable #-}

module Graphics.BokeHS.Serialize where

import Data.Text (pack, Text)
import GHC.Exts (fromList)
import Control.Monad.State
import Data.Aeson
import qualified Data.HashMap.Lazy as HML
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as CS
import Data.Aeson.Encode.Pretty (encodePretty)
import Graphics.BokeHS.Models
import Paths_bokeHS

bokehVersion :: Text
bokehVersion = "0.12.16"

mergeAeson :: [Value] -> Value
mergeAeson = Object . HML.unions . map (\(Object o) -> o)

l2o :: [(Text, Value)] -> Value
l2o = Object . fromList

--holds serialization state
data SerialEnv = SerialEnv {
        nodes :: [BNode], --list of nodes in the graph
        idCount :: Int --current BID generation state 
    } deriving Show

-- | Class for types that can be serialized to BokehJS.
-- Can either be a primitive type whose value can be directly inserted (makePrim).
-- Or a ref, trivially constructed from the typename, is inserted in place of the
-- value, and the value itself is added to the node list of the BokehJS graph.

class Bokeh a where
    makePrim :: a -> Value
    makePrim _ = Null
    
    serializeNode :: a -> State SerialEnv Value    
    serializeNode o = return $ makePrim o
    {-# MINIMAL makePrim | serializeNode #-}



-- | Helper function for getting new BIDs
newBID :: State SerialEnv BID
newBID = do
    env <- get
    let curID = idCount env
    put env{idCount = curID + 1}
    return $ (BID . pack . show) curID -- FIXME? this doesn' return the updated ID 

-- | Helper function for adding nodes to the node list
addNode :: BNode -> State SerialEnv ()
addNode newVal = do
    env <- get
    let vals = nodes env
    put env{nodes=newVal : vals}

-- | helper function for conveniently turning a list of attributes into a seralizing function
type Attributes = [(Text, Value)]
makeRef :: BType -> Attributes -> State SerialEnv Value
makeRef btype attrs = do
    curID <- newBID
    let footer = l2o [("id", toJSON curID), ("type", toJSON btype)]
        attrObj = l2o [("attributes", (Object . fromList) attrs)]
        node = (BNode . mergeAeson) [footer, attrObj]
    addNode node
    return footer
instance Bokeh Plot where
    serializeNode plt = snd <$> serializePlot plt



instance Bokeh Placeholder where
    makePrim (Placeholder v)= v

instance (Bokeh a) => Bokeh (Maybe a) where
  serializeNode m = case m of
    Nothing -> pure Null
    Just o -> serializeNode o

instance Bokeh Color where
    makePrim color = toJSON colorString
        where
            colorString :: Text
            colorString = case color of
                Purple -> "purple"
                White -> "white"
                Lavender -> "lavender"

instance Bokeh Title where
    serializeNode (Title titletext) = makeRef (BType "Title") titleObj
            where
                 titleObj = [
                        ("plot", Null), 
                        ("text", String titletext)
                    ]
data AxisWrapper = AxWrap Value Axis

instance Bokeh AxisWrapper where
    --TODO: Validate Parent Value
    serializeNode  (AxWrap parentRef (LinearAxis form tick)) = do
        formatter_ <- serializeNode form 
        ticker_ <- serializeNode tick
        let axisObj = [("formatter", formatter_), ("plot", parentRef), ("ticker", ticker_)]
        makeRef (BType "LinearAxis") axisObj

instance Bokeh DataSource where
    serializeNode cds = do
        selected_ <- serializeNode (selected cds)
        selectionPolicy_ <- serializeNode (selectionPolicy cds)
        let cdsObj = [("callback", Null), ("data", dataObj), 
                ("selected", selected_), ("selection_policy", selectionPolicy_)]
        makeRef (BType "ColumnDataSource") cdsObj
        where toObj (Field ftext, nums) = (ftext, toJSON nums)
              dataObj = (Object . fromList) $ toObj <$> cols cds

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

instance Bokeh ViewWrapper where
    serializeNode (VWrap cdsRef CDSView) = makeRef (BType "CDSView") viewObj
        where viewObj = [("source", cdsRef)]
    serializeNode _ = undefined  -- FIXME

instance Bokeh Scale where
    serializeNode LinearScale = makeRef (BType "LinearScale") []

instance Bokeh Ticker where
    serializeNode BasicTicker = makeRef (BType "BasicTicker") []

instance Bokeh Formatter where
    serializeNode BasicTickFormatter = makeRef (BType "BasicTickFormatter") []

instance Bokeh Range where
    serializeNode range = makeRef (BType "Range1d") 
        [("callback", Null), ("start", toJSON (start range)), ("end", toJSON (end range))]

instance Bokeh SelectionPolicy where
    serializeNode UnionRenderers = makeRef (BType "UnionRenderers") []
    serializeNode _ = undefined -- FIXME

instance Bokeh Selection where
    serializeNode Selection = makeRef (BType "Selection") []
    serializeNode _ = undefined -- FIXME

instance Bokeh Glyph where
    serializeNode (Line color x y) = makeRef (BType "Line") [("line_color", makePrim color), 
        ("x", l2o [("field", toJSON x)]),("y", l2o [("field", toJSON y)])]

instance (Bokeh a) => Bokeh (Auto a) where
  serializeNode aa = case aa of
    Auto -> pure $ String "auto"
    NotAuto o -> serializeNode o

instance Bokeh Toolbar where
    serializeNode bar = do
        active_drag_ <- serializeNode (activeDrag bar)
        active_inspect_ <- serializeNode (activeInspect bar)
        active_scroll_ <- serializeNode (activeScroll bar)
        active_tap_ <- serializeNode (activeTap bar)
        let barObj = [("active_drag", active_drag_), ("active_scroll", active_scroll_),
                ("active_inspect", active_inspect_), ("active_tap", active_tap_)]
        makeRef (BType "Toolbar") barObj

serializePlot :: Plot -> State SerialEnv (BID, Value)
serializePlot plt@Plot{height = plot_height, width = plot_width} = do
        let curID = BID "plot_id"
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

makeBokeh :: Plot -> Value
makeBokeh plt = let
    (go, endState) = runState (serializePlot plt) (SerialEnv [] 0)
    (pid, _) = go
    topObj = l2o [("top_obj", containerObj)]
    containerObj = l2o [
        ("roots", rootObj),
        ("title", String "BokeHS Application"),
        ("version", String bokehVersion)
        ]
    rootObj = l2o [
        ("references", toJSON $ nodes endState),
        ("root_ids", toJSON [pid])
        ]
    in topObj

printVal :: Value -> IO ()
printVal = BS.putStr . encodePretty

--very messy, replace with a templating engine
emitPlotHTML :: Plot -> IO BS.ByteString
emitPlotHTML plot = do
    file <- getDataFileName "templates/bokeh.tpl"
    template <- BS.readFile file
    let [bef, aft] = CS.split '$' template
    return $ bef `mappend` (encode . makeBokeh) plot `mappend` aft

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

defaultPlot :: Plot
defaultPlot = Plot{
       backgroundFill = Lavender,
       width = 400,
       height = 400,
       renderers = [xaxis, yaxis, lrend],
       title = Title "Sample bokeh-hs plot",
       toolbar = defaultToolbar,
       xRange = Range1d (-0.5) 20,
       yRange = Range1d (-0.5) 20,
       xScale = LinearScale,
       yScale = LinearScale
    } where
        xaxis = ARend BBelow ax
        yaxis = ARend BLeft ax
        ax = LinearAxis{formatter=BasicTickFormatter, ticker=BasicTicker}
        lrend = GRend GlyphRenderer { hoverGlyph = Nothing, mutedGlyph = Nothing,
        dataSource = samplesrc, glyph = lin, vie = CDSView}
        lin = Line Purple (Field "x") (Field "y")

