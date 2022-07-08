module Common where

import YamlParser

import Data.List as DL
import Data.List.Split as DLS
import Data.Time


{-Amalgamation functions.-}

amalgamate2DGrid :: FFConfig              ->
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

amalgamate2DGridESmall :: [(Int,Int,Int,Int)] ->
                          [[(Int,Int,Int,Int)]] ->
                          [[(Int,Int,Int,Int)]]
amalgamate2DGridESmall [] [] = []
amalgamate2DGridESmall _  [] = []
amalgamate2DGridESmall [] _  = []
amalgamate2DGridESmall xs ys =
  amalgamatedys 
    where
      amalgamatedys = DL.map
                      (DL.map (\(e,f,g,h) -> if | (e,f) `DL.elem` ab
                                                -> (\(x:_) -> x) $
                                                   DL.filter (\(j,k,_,_) -> j == e &&
                                                                            k == f
                                                             )
                                                   xs
                                                | otherwise
                                                -> (e,f,g,h)
                              )
                      ) ys
      ab            = DL.map (\(a,b,_,_) -> (a,b))
                      xs

amalgamate2DGridE :: [[(Int,Int,Int,Int)]] ->
                     [[(Int,Int,Int,Int)]] ->
                     [[[(Int,Int,Int,Int)]]]
amalgamate2DGridE []     [] = []
amalgamate2DGridE _      [] = []
amalgamate2DGridE []     _  = []
amalgamate2DGridE (x:xs) ys =
  (amalgamate2DGridESmall x ys) : (amalgamate2DGridE xs ys)  

{-------------------------}


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
