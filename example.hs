{-# LANGUAGE OverloadedStrings #-}
import Graphics.BokeHS.Models
import Graphics.BokeHS.Serialize
import Data.ByteString.Lazy as BS
import Control.Monad
import System.Process

myData :: DataSource
myData = CDS {
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
                     10.416666666666667,
                     2.583333333333334,
                     7.75,
                     5.916666666666668,
                     19.083333333333334,
                     13.25] 


myPlot :: Plot
myPlot = Plot{
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
        dataSource = myData, glyph = lin, vie = CDSView}
        lin = Line Purple (Field "x") (Field "y")

main :: IO ()
main = do
    plotHTML <- emitPlotHTML myPlot
    BS.writeFile "sample.html" plotHTML
    void $ system "firefox --new-window sample.html"    
    -- void $ system "icecat --new-window sample.html"
