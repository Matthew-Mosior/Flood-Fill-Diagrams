module DiagramGeneration.Generate2DGrid where


import YamlParser

import Data.List as DL
import Data.List.Split as DLS
import Data.Ord
import Data.Text as DText
import Data.Vector as DV
import Diagrams.Combinators
import Diagrams.Prelude
import Diagrams.TwoD
import Diagrams.TwoD.Text
import Diagrams.TwoD.Types
import Diagrams.Size
import Diagrams.Backend.SVG
import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Backend.Rasterific
import System.IO as SIO
import System.Random.MWC


--Define the 2D grid.
type Square = (Int,Int)

grid2DSquare (c,d) = if | c == 3
                        -> text (show d) #
                           scale 0.5     <>
                           unitSquare    #
                           lw thin       #
                           fc white
                         | c /= 2 &&
                           c /= 3
                         -> unitSquare #
                            lw thin    #
                            fc white
                         | otherwise
                         -> unitSquare    # 
                            lw thin       #
                            fc midnightblue

grid2DSVG :: Square 
          -> [[(Int,Int,Int,Int)]]
          -> Diagram Diagrams.Backend.SVG.B
grid2DSVG (x,y) v = do 
  frame <> lattice
    where tolistvf        = DL.map (DL.map (\(_,_,c,d) -> (c,d))) v
          lattice         = centerXY    .
                            vcat        . 
                            DL.map hcat .
                            (DL.map . DL.map) grid2DSquare $ tolistvf
          frame           = rect (fromIntegral y) (fromIntegral x)
                            # lw thick 

floodFillDiagramsSVG :: FFConfig
                     -> [[(Int,Int,Int,Int)]]
                     -> IO ()
floodFillDiagramsSVG config random2dgenl = do
  let svg = frame 10 $ grid2DSVG (numrows config,numcols config)
                                 random2dgenl
  renderSVG (DText.unpack $
            outputpathsvg config)
            (dims $ V2 1000 1000)
            svg

grid2DGIFSmall :: Square
               -> [[(Int,Int,Int,Int)]]
               -> Diagram Diagrams.Backend.Rasterific.B
grid2DGIFSmall (x,y) v = do
  frame <> lattice
    where tolistvf        = DL.map (DL.map (\(_,_,c,d) -> (c,d))) v
          lattice         = centerXY    .
                            vcat        .
                            DL.map hcat .
                            (DL.map . DL.map) grid2DSquare $ tolistvf
          frame           = rect (fromIntegral y) (fromIntegral x)
                            # lw thick 

grid2DGIF :: Square
          -> [[[(Int,Int,Int,Int)]]]
          -> [Diagram Diagrams.Backend.Rasterific.B]
grid2DGIF _     []     = []
grid2DGIF (x,y) (v:vs) =
  grid2DGIFSmall (x,y) v :
  grid2DGIF (x,y) vs

floodFillDiagramsGIF :: FFConfig
                     -> [[[(Int,Int,Int,Int)]]]
                     -> IO ()
floodFillDiagramsGIF config random2dgengif = do
  let gif = grid2DGIF (numrows config,numcols config)
                      random2dgengif
  animatedGif (DText.unpack $
              outputpathgif config)
              (dims $ V2 500 500)
              LoopingForever
              100
              gif
