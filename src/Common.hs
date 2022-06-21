module Common where

import YamlParser

import Data.List as DL
import Data.List.Split as DLS
import Data.Time


{-Amalgamation function.-}

amalgamate2DGrid :: FFConfig ->
                    [[(Int,Int,Int,Int)]] ->
                    [[(Int,Int,Int,Int)]] ->
                    [[(Int,Int,Int,Int)]]
amalgamate2DGrid config xs ys = 
  DLS.chunksOf (numcols config) amalgamated
    where
      amalgamated = DL.map (\(x@(_,_,c,d),y@(_,_,g,_)) -> if | g == 3
                                                             -> y
                                                             | g /= 3
                                                             -> x
                                                             | otherwise
                                                             -> x 
                           )
                    zipped 
      zipped = DL.zip (DL.concat xs)
                      (DL.concat ys)  

{------------------------}


{-Time related functions.-}

showPrettyZonedTime :: ZonedTime -> String
showPrettyZonedTime currenttandd =
  currenttanddnotimezone DL.++
  zeroestoadd            DL.++
  " "                    DL.++
  currenttimezone
    where
      zeroestoadd            = DL.concat                                               $
                               DL.map show                                             $
                               DL.take (26 - (DL.length currenttanddnotimezone))       $
                               DL.repeat 0
      currenttanddnotimezone = DL.reverse
                               (DL.drop 4
                               (DL.reverse
                               (show currenttandd)))
      currenttimezone        = DL.reverse
                               (DL.take 3
                               (DL.reverse
                               (show currenttandd)))

{-------------------------}
