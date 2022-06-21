module Main where


import RunFF
import CmdOpts
import Common

import Data.List as DL
import Data.Time as DTime
import System.Directory as SD
import System.Environment as SE
import System.Exit as SX
import System.IO as SIO


{-Main function.-}

main :: IO ()
main = do
  --Get command line arguments.
  (args,files) <- SE.getArgs >>= compilerOpts
  --See if files is null.
  if | (DL.length files) /= 1
     -> do --Print out Flood Fill ascii art.
           _ <- SIO.putStrLn ff3DMacroFont
           --Print error statement and exit.
           currenttandd <- DTime.getZonedTime
           SIO.putStrLn ("[" DL.++ (showPrettyZonedTime currenttandd) DL.++ "] "
                             DL.++ "FF requires one argument:\n\
                                   \Argument 1:Configuration YAML file")
           currenttandd <- DTime.getZonedTime
           SIO.putStrLn ("[" DL.++ (showPrettyZonedTime currenttandd) DL.++ "] "
                             DL.++ "Shutting down Flood-Fill v0.1.0.0 ...")
           SX.exitWith (SX.ExitFailure 1)
     | otherwise
     -> do --Print out Flood Fill ascii art.
           _ <- SIO.putStrLn ff3DMacroFont
           --Run args and files through processArgsAndFiles.
           runFloodFillDiagrams (args,files) 

{----------------}
