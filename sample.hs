{-# LANGUAGE OverloadedStrings #-}
import Graphics.BokeHS
import Data.ByteString.Lazy as BS
import Control.Monad
import System.Process

myPlot :: Plot
myPlot = plt
    $> addLinearAxis BBelow
    |> addLinearAxis BLeft
    |> addLine myData fst snd green
    |> addLine myData fst (const 5) blue
    |> addLine myData2 fst snd red
    where plt = defaultPlot{
            width = 1000,
            height = 1000,
            title = "Sample BokeHS plot",
            xRange = Range1d (-0.5) 20,
            yRange = Range1d (-0.5) 20 
        }

main :: IO ()
main = do
    plotHTML <- emitPlotHTML myPlot
    BS.writeFile "sample.html" plotHTML
    void $ system "firefox --new-window sample.html"    

myData :: [(BNum, BNum)]
myData = Prelude.zip xcols ycols
    where 
            xcols = [-0.5,
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

myData2 :: [(BNum, BNum)]
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
