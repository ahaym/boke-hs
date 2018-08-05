{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AutoDeriveTypeable #-}

module Graphics.BokeHS.Serialize where

import Data.Text (pack, Text)
import GHC.Exts (fromList)
import GHC.Generics
import Control.Monad.State
import Data.Aeson
import qualified Data.Colour.SRGB as C
import qualified Data.HashMap.Lazy as HML
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as CS
import Data.Aeson.Encode.Pretty (encodePretty)

import Graphics.BokeHS.Models
import Graphics.BokeHS.GlyphConfig
import Graphics.BokeHS.Prim
import Paths_bokeHS

bokehVersion :: Text
bokehVersion = "0.12.16"

mergeAeson :: [Value] -> Value
mergeAeson = Object . HML.unions . map (\(Object o) -> o)

l2o :: [(Text, Value)] -> Value
l2o = Object . fromList

newtype BNode = BNode Value deriving (Show, Generic)
instance ToJSON BNode 

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
    let newID = succ $ idCount env
    put env{idCount = newID}
    return $ (BID . pack . show) newID

-- | Helper function for adding nodes to the node list
addNode :: BNode -> State SerialEnv ()
addNode newVal = do
    env <- get
    let vals = nodes env
    put env{nodes=newVal : vals}

-- | helper function for conveniently turning a list of attributes into a serializing function
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

instance (Bokeh l, Bokeh r) => Bokeh (Either l r) where
    serializeNode v = case v of
        Left a -> serializeNode a
        Right b -> serializeNode b

instance (Bokeh a) => Bokeh (Auto a) where
  serializeNode aa = case aa of
    Auto -> pure $ String "auto"
    NotAuto o -> serializeNode o

--is equal to Models.Color
instance Bokeh (C.Colour Double) where
    makePrim = toJSON .  C.sRGB24show

instance Bokeh Angle where
    makePrim (Angle a) = toJSON a

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
    serializeNode CDS{..} = do
        selected_ <- serializeNode selected
        selectionPolicy_ <- serializeNode selectionPolicy
        let cdsObj = [("callback", Null), ("data", dataObj), 
                ("selected", selected_), ("selection_policy", selectionPolicy_)]
        makeRef (BType "ColumnDataSource") cdsObj
        where toObj (Field ftext, nums) = (ftext, toJSON nums)
              dataObj = (Object . fromList) $ toObj <$> cols

instance Bokeh GlyphRenderer where
    serializeNode GlyphRenderer{..} = do
        hover_glyph_ <- serializeNode hoverGlyph
        muted_glyph_ <- serializeNode mutedGlyph
        data_source_ <- serializeNode dataSource
        glyph_ <- serializeNode glyph 
        view_ <- serializeNode (VWrap data_source_ vie)
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
    serializeNode Range1d{..} = makeRef (BType "Range1d") 
        [("callback", Null), ("start", toJSON start), ("end", toJSON end)]

instance Bokeh SelectionPolicy where
    serializeNode UnionRenderers = makeRef (BType "UnionRenderers") []
    serializeNode _ = undefined -- FIXME

instance Bokeh Selection where
    serializeNode Selection = makeRef (BType "Selection") []
    serializeNode _ = undefined -- FIXME

instance Bokeh Glyph where
    serializeNode (Line config x y) = makeRef (BType "Line") $
        [ ("x", l2o [("field", toJSON x)])
        , ("y", l2o [("field", toJSON y)])
        ] ++ mkConfig config

instance Bokeh Toolbar where
    serializeNode Toolbar{..} = do
        active_drag_ <- serializeNode activeDrag
        active_inspect_ <- serializeNode activeInspect
        active_scroll_ <- serializeNode activeScroll
        active_tap_ <- serializeNode activeTap
        let barObj = [("active_drag", active_drag_), ("active_scroll", active_scroll_),
                ("active_inspect", active_inspect_), ("active_tap", active_tap_)]
        makeRef (BType "Toolbar") barObj

serializePlot :: Plot -> State SerialEnv (BID, Value)
serializePlot Plot{..} = do
        let curID = BID "plot_id"
        let footer = l2o [(pack "id", toJSON curID), (pack "type", toJSON $ BType"Plot")]
        background_fill_ <- serializeNode backgroundFill 
        title_ <- serializeNode title 
        toolbar_ <- serializeNode toolbar 
        x_range_ <- serializeNode xRange 
        y_range_ <- serializeNode yRange 
        x_scale_ <- serializeNode xScale 
        y_scale_ <- serializeNode yScale  
        renderers_ <- mapM (serializeRend footer) renderers 
        let lefts = (toJSON . map snd) $ filter (dPred BLeft) renderers_
            rights = (toJSON . map snd) $ filter (dPred BRight) renderers_
            aboves = (toJSON . map snd) $ filter (dPred BAbove) renderers_
            belows = (toJSON . map snd) $ filter (dPred BBelow) renderers_
            plotAttrs = l2o [
                ("background_fill_color", l2o [("value", background_fill_)]),
                ("title", title_),
                ("plot_height", toJSON height),
                ("plot_width", toJSON width),
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

