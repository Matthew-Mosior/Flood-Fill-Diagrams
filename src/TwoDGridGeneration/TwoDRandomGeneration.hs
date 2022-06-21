module TwoDGridGeneration.TwoDRandomGeneration where


import YamlParser

import Data.List as DL
import Data.List.Split as DLS
import Data.Vector as DV
import Data.Ix
import System.IO as SIO
import System.Random.MWC
import System.Random.Stateful


{-Generate 2D grid of random values.-}

grabInitialXYCoord :: [[(Int,Int,Int,Int)]] -> IO (Int,Int)
grabInitialXYCoord [] = return (-1,-1)
grabInitialXYCoord xs = do
  g <- createSystemRandom
  v <- uniformRM (0,(DL.length (DL.concat xs)) - 1) g
  if | ((\(_,_,c,_) -> c) $
       (DL.concat xs) DL.!! v) == 2 
     -> grabInitialXYCoord xs
     | otherwise
     -> do let initialxy = (\(a,b,_,_) -> (a,b)) $
                           (DL.concat xs) DL.!! v
           return initialxy

generateRandom2DGrid :: FFConfig -> IO [[(Int,Int,Int,Int)]]
generateRandom2DGrid config = do
  g <- createSystemRandom
  random2Dgrid <- replicateM (numrows config) $
                  replicateM (numcols config) $
                  uniformRM (1,2 :: Integer) g
  let random2Dgridp = DL.map (DL.map (fromIntegral)) $
                      DL.map (DV.toList)             $
                      DV.toList random2Dgrid 
  let gridrange = DL.map (DL.map (\(a,b) -> (b,a))) $
                  DLS.chunksOf (numcols config)
                  (range ((0,0),((numrows config) - 1,(numcols config) - 1))) 
  let random2Dgridfinal = DL.map (DL.map (\((a,b),c) -> (a,b,c,-1))) $
                          DL.zipWith (\a b -> DL.zip a b) gridrange
                                                          random2Dgridp 
  return random2Dgridfinal

{------------------------------------}
