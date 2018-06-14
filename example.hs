{-# LANGUAGE OverloadedStrings #-}
import Graphics.BokeHS.Models
import Graphics.BokeHS.Helpers
import Graphics.BokeHS.Serialize (emitPlotHTML, defaultToolbar)
import Data.ByteString.Lazy as BS
import Control.Monad
import System.Process
import Data.Colour.Names

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

myData2 = Prelude.zip xcols ycols
        where
            xcols =  [-0.5,
                     1.8333333333333335,
                     4.166666666666667,
                     6.5,
                     8.833333333333334,
                     11.166666666666668,
                     13.5,
                     15.833333333333336,
                     18.166666666666668,
                     20.5]
            ycols = [3.75,
                     4.916666666666667,
                     6.083333333333334,
                     7.25,
                     8.416666666666667,
                     9.583333333333334,
                     10.75,
                     11.916666666666668,
                     13.083333333333334,
                     14.25] 



myPlot :: Plot
myPlot = Plot{
       backgroundFill = white,
       width = 1000,
       height = 1000,
       renderers = [xaxis, yaxis, lrend],
       title = Title "Sample bokeHS plot",
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
        lin = Line green (Field "x") (Field "y")

myPlot2 :: Plot
myPlot2 = addLine myPlot myData2 fst snd red

main :: IO ()
main = do
    plotHTML <- emitPlotHTML myPlot2
    BS.writeFile "sample.html" plotHTML
    void $ system "firefox --new-window sample.html"    
