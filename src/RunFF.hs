module RunFF where


import CmdOpts
import Common
import DiagramGeneration.Generate2DGrid
import TwoDGridGeneration.TwoDRandomGeneration
import ScanlineStackFloodFill.FloodFill
import YamlParser

import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.Array.MArray
import Data.STRef
import Data.ByteString.Char8 as DBSC8
import Data.Foldable
import Data.List as DL
import Data.List.Split as DLS
import Data.Maybe as DMaybe
import Data.Text as DText
import Data.Time as DTime
import Data.Typeable
import Data.Yaml as DYaml
import System.Directory as SD
import System.Environment as SE
import System.Exit as SX
import System.IO as SIO
import Text.RawString.QQ as TRQQ


{-3D Macro Font.-}

ff3DMacroFont :: String
ff3DMacroFont =
  [r|
           ________                 __   _______ ____
          / ____/ /____  ____  ____/ /  / ____(_) / /
         / /_  / // __ \/ __ \/ __  /  / /_  / / / / 
        / __/ / // /_/ / /_/ / /_/ /  / __/ / / / /  
       /_/   /_/ \____/\____/\__,_/  /_/   /_/_/_/   
                    ____    ___  ____    ____ 
             _   __/ __ \  <  / / __ \  / __ \
            | | / / / / /  / / / / / / / / / /
            | |/ / /_/ /_ / /_/ /_/ /_/ /_/ / 
            |___/\____/(_)_/(_)____/(_)____/

          Copyright (c) Matthew C. Mosior 2022
  |]

{----------------}


{-FF specific functions.-}

nonNegativeNumofRows :: FFConfig -> Bool
nonNegativeNumofRows config =
  if | numrows config <= 0
     -> False
     | otherwise
     -> True

nonNegativeNumofCols :: FFConfig -> Bool
nonNegativeNumofCols config =
  if | numcols config <= 0
     -> False
     | otherwise
     -> True

processConfigurationYaml :: FFConfig -> IO Bool
processConfigurationYaml config = 
  if | nonNegativeNumofRows config &&
       nonNegativeNumofCols config
     -> return True 
     | otherwise
     -> return False

runFloodFillDiagrams :: ([Flag],[String]) -> IO ()
runFloodFillDiagrams ([],[]) = return ()
runFloodFillDiagrams (_,inputfiles) = do
  --Read in configuration Yaml.
  readinputyaml <- DBSC8.readFile ((\(x:_) -> x) inputfiles)
  --Decode readinputyaml.
  decodedinputyaml <- decodeThrow readinputyaml :: IO FFConfig
  --Process and ensure that Configuration YAML is properly sanitized/formatted.
  processedConfig <- processConfigurationYaml decodedinputyaml
  if | processedConfig
     -> do --Generate random X x Y grid.
           currenttandd <- DTime.getZonedTime
           _ <- SIO.putStrLn ("[" DL.++ (showPrettyZonedTime currenttandd) DL.++ "] "
                                  DL.++ "Generating random "
                                  DL.++ (show $ numcols decodedinputyaml) DL.++ " X "
                                  DL.++ (show $ numrows decodedinputyaml)
                                  DL.++ " grid ...")
           random2dgen <- generateRandom2DGrid decodedinputyaml
           --Grab initial X Y coordinate.
           currenttandd <- DTime.getZonedTime
           _ <- SIO.putStrLn ("[" DL.++ (showPrettyZonedTime currenttandd) DL.++ "] "
                                  DL.++ "Grab initial X Y coordinates ...")
           initialxy <- grabInitialXYCoord random2dgen
           --Run scanline stack flood fill algorithm.
           currenttandd <- DTime.getZonedTime
           _ <- SIO.putStrLn ("[" DL.++ (showPrettyZonedTime currenttandd) DL.++ "] "
                                  DL.++ "Run scanline stack flood fill algorithm on 2D grid using initial X Y coordinates ...")
           let random2dgenf = runSTArray $ floodFillScanlineStack decodedinputyaml
                                                                  (DL.concat random2dgen)
                                                                  initialxy               
                                                                  3
           --Convert random2dgenf into list.
           currenttandd <- DTime.getZonedTime
           _ <- SIO.putStrLn ("[" DL.++ (showPrettyZonedTime currenttandd) DL.++ "] "
                                  DL.++ "Converting STArray to list ...")
           let random2dgenfl = DLS.chunksOf (numcols decodedinputyaml)
                                            (elems random2dgenf) 
           --Combine original randomly generated 2d grid with scanline stack flood fill algorithm result.
           currenttandd <- DTime.getZonedTime
           _ <- SIO.putStrLn ("[" DL.++ (showPrettyZonedTime currenttandd) DL.++ "] "
                                  DL.++ "Amalgamating original random 2D grid with scanline stack flood fill algorithm resultant list ...")
           let final2dgen = amalgamate2DGrid decodedinputyaml
                                             random2dgen 
                                             random2dgenfl 
           --Run floodFill on configuration yaml inputs
           currenttandd <- DTime.getZonedTime
           _ <- SIO.putStrLn ("[" DL.++ (showPrettyZonedTime currenttandd) DL.++ "] "
                                  DL.++ "Generate final SVG ...")
           floodFillDiagrams decodedinputyaml
                             final2dgen
           --Shut down Flood-Fill v0.1.0.0.
           _ <- SIO.putStrLn ("[" DL.++ (showPrettyZonedTime currenttandd) DL.++ "] "
                                  DL.++ "Shutting down Flood-Fill v0.1.0.0 ...")
           SX.exitWith (SX.ExitSuccess)
     | otherwise
     -> do --Print out failure message.
           currenttandd <- DTime.getZonedTime
           _ <- SIO.putStrLn ("[" DL.++ (showPrettyZonedTime currenttandd) DL.++ "] "
                                  DL.++ "Could not sanitize Configuration YAML ...")
           currenttandd <- DTime.getZonedTime
           --Shut down Flood-Fill v0.1.0.0.
           _ <- SIO.putStrLn ("[" DL.++ (showPrettyZonedTime currenttandd) DL.++ "] "
                                  DL.++ "Shutting down Flood-Fill v0.1.0.0 ...")
           SX.exitWith (SX.ExitFailure 1)

{------------------------}
