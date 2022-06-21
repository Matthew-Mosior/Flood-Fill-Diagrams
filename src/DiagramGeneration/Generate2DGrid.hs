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

grid2D :: Square -> [[(Int,Int,Int,Int)]] -> Diagram B
grid2D (x,y) v = do 
  frame <> lattice
    where tolistvf        = DL.map (DL.map (\(_,_,c,d) -> (c,d))) v
          lattice         = centerXY    .
                            vcat        . 
                            DL.map hcat .
                            (DL.map . DL.map) grid2DSquare $ tolistvf
          frame           = rect (fromIntegral y) (fromIntegral x)
                            # lw thick 

floodFillDiagrams :: FFConfig -> [[(Int,Int,Int,Int)]] -> IO () 
floodFillDiagrams config random2dgenl = do
  let example2dgrid = frame 10 $ grid2D (numrows config,numcols config)
                                        random2dgenl
  renderSVG (DText.unpack $ 
             outputpath config) 
            (dims $ V2 1000 1000)
            example2dgrid
