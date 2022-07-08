module ScanlineStackFloodFill.FloodFill where


import YamlParser

import Data.List as DL
import Data.Array
--import Data.Array.Base
import Data.Array.ST
import Data.Array.MArray
import Data.Foldable
import Data.Ord
import Data.STRef
import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict

-- | The core of this implementation is borrowed from code shared on Rosetta code:
-- https://rosettacode.org/wiki/Bitmap/Flood_fill#Haskell 
 
{-Implementation of a stack in the ST monad-}

-- | Abstract Stack data type
type Stack a = [a]
type STStack s a = STRef s (Stack a)

pushST :: STStack s a -> a -> ST s ()
pushST s e = do
  s2 <- readSTRef s
  writeSTRef s (e:s2)

popST :: STStack s a -> ST s (Stack a)
popST s = do
  s2 <- readSTRef s
  writeSTRef s $ tail s2
  return $ take 1 s2
 
isNotEmptySTStack :: STStack s a -> ST s Bool
isNotEmptySTStack s = do
  s2 <- readSTRef s
  return $ not $ null s2
 
emptySTStack :: ST s (STStack s a)
emptySTStack = newSTRef []
 
consumeSTStack :: STStack s a -> (a -> ST s ()) -> ST s ()
consumeSTStack s f = do
  check <- isNotEmptySTStack s
  when check $ do
    e <- popST s
    f $ head e
    consumeSTStack s f
 
type Spanning s = STRef s (Bool, Bool)
 
setSpanLeft :: Spanning s -> Bool -> ST s ()
setSpanLeft s v = do
  (_,r) <- readSTRef s
  writeSTRef s (v,r)
 
setSpanRight :: Spanning s -> Bool -> ST s ()
setSpanRight s v = do
  (l,_) <- readSTRef s
  writeSTRef s (l,v)
 
setSpanNone :: Spanning s -> ST s ()
setSpanNone s = writeSTRef s (False,False)

{-------------------------------------------}


{-Scanline stack flood fill algorithm.-}

floodFillScanlineStack :: ( Eq Int
                          , MArray (STArray s) (Int,Int,Int,Int) (ST s) 
                          , Num Int 
                          ) => FFConfig -> [(Int,Int,Int,Int)] -> (Int,Int) -> Int -> ST s (STArray s Int (Int,Int,Int,Int)) 
floodFillScanlineStack config b coords newC = do 
  rstack <- emptySTStack -- new empty stack
  fstack <- newArray (0,(DL.length b) - 1) (-1,-1,-1,-1) -- new empty array to holding final filled grid (final array)
  spans <- newSTRef (False,False) -- keep track of spans in scanWhileX 
  fFSS b rstack fstack coords newC spans -- function loop
  return fstack
    where  
      fFSS b rst ffst c@(initialx,initialy) newC p = do
        let oldC = (\(_,_,old,_) -> old)                                    $
                   (\(x:_) -> x)                                            $
                   DL.filter (\(x,y,_,_) -> x == initialx && y == initialy) b
        unless (oldC == newC) $ do
          pushST rst (initialx,initialy) -- store the coordinates in the running stack 
          consumeSTStack rst (scanWhileY b p oldC ffst >=>
            scanWhileX b rst p oldC newC ffst (numcols config,numrows config))
 
      -- take a buffer, the span record, the color of the Color the fill is
      -- started from, a coordinate from the stack, and returns the coord
      -- of the next point to be filled in the same column
      scanWhileY b p oldC ffst coords@(currentx,currenty) =
        if | currenty >= 0
           -> do let currenti = (currenty - 0) * (((numcols config) - 1) - 0 + 1) + (currentx - 0)
                 currentie <- readArray ffst currenti 
                 let z = if | (\(_,_,c,_) -> c) currentie == -1
                            -> (\(_,_,old,_) -> old)                                    $
                               (\(x:_) -> x)                                            $
                               DL.filter (\(x,y,_,_) -> x == currentx && y == currenty) b
                            | otherwise
                            -> 2
                 if | z == oldC
                    -> scanWhileY b p oldC ffst (currentx,currenty - 1)
                    | otherwise
                    -> do setSpanNone p
                          return (currentx,currenty + 1)
           | otherwise
           -> do setSpanNone p
                 return (currentx,currenty + 1)
 
      -- take a buffer, a stack, a span record, the old and new color, the
      -- height and width of the buffer, and a coordinate.
      -- paint the point with the new color, check if the fill must expand
      -- to the left or right or both, and store those coordinates in the
      -- stack for subsequent filling
      scanWhileX b rst p oldC newC ffst (w,h) coords@(currentx,currenty) =
        when (currenty < h) $ do
          let currenti = (currenty - 0) * (((numcols config) - 1) - 0 + 1) + (currentx - 0)
          currentie <- readArray ffst currenti
          let z = if | ((\(_,_,c,_) -> c) currentie) == -1 
                     -> (\(_,_,old,_) -> old)                                    $
                        (\(x:_) -> x)                                            $
                        DL.filter (\(x,y,_,_) -> x == currentx && y == currenty) b
                     | otherwise
                     -> 2 
          when (z == oldC) $ do
            ffstl <- getElems ffst                                       
            let newmax = if | ((\(_,_,_,d) -> d) $
                              (maximumBy (\(_,_,_,d) (_,_,_,h) -> compare d h) ffstl)) == -1
                            -> 0
                            | otherwise
                            -> ((\(_,_,_,d) -> d) $
                               (maximumBy (\(_,_,_,d) (_,_,_,h) -> compare d h) ffstl)) + 1
            writeArray ffst currenti (currentx,currenty,newC,newmax) -- store the coordinates in the final stack 
            (spanLeft,spanRight) <- readSTRef p
            when (not spanLeft && currentx > 0) $ do
              let currenti = (currenty - 0) * (((numcols config) - 1) - 0 + 1) + ((currentx - 1) - 0)
              currentie <- readArray ffst currenti
              let z2 = if | ((\(_,_,c,_) -> c) currentie) == -1 
                          -> (\(_,_,old,_) -> old)                                    $
                             (\(x:_) -> x)                                            $
                             DL.filter (\(x,y,_,_) -> x == (currentx - 1) && y == currenty) b
                          | otherwise
                          -> 2 
              when (z2 == oldC) $ do
                pushST rst (currentx - 1,currenty)
                setSpanLeft p True
            when (spanLeft && currentx > 0) $ do
              let currenti = (currenty - 0) * (((numcols config) - 1) - 0 + 1) + ((currentx - 1) - 0)
              currentie <- readArray ffst currenti 
              let z3 = if | ((\(_,_,c,_) -> c) currentie) == -1 
                          -> (\(_,_,old,_) -> old)                                    $
                             (\(x:_) -> x)                                            $
                             DL.filter (\(x,y,_,_) -> x == (currentx - 1) && y == currenty) b
                          | otherwise
                          -> 2 
              when (z3 /= oldC) $
                setSpanLeft p False
            when (not spanRight && currentx < (w - 1)) $ do
              let currenti = (currenty - 0) * (((numcols config) - 1) - 0 + 1) + ((currentx + 1) - 0)
              currentie <- readArray ffst currenti 
              let z4 = if | ((\(_,_,c,_) -> c) currentie) == -1 
                          -> (\(_,_,old,_) -> old)                                    $
                             (\(x:_) -> x)                                            $
                             DL.filter (\(x,y,_,_) -> x == (currentx + 1) && y == currenty) b
                          | otherwise
                          -> 2 
              when (z4 == oldC) $ do
                pushST rst (currentx + 1,currenty)
                setSpanRight p True
            when (spanRight && currentx < (w - 1)) $ do
              let currenti = (currenty - 0) * (((numcols config) - 1) - 0 + 1) + ((currentx + 1) - 0)
              currentie <- readArray ffst currenti 
              let z5 = if | ((\(_,_,c,_) -> c) currentie) == -1
                          -> (\(_,_,old,_) -> old)                                    $
                             (\(x:_) -> x)                                            $
                             DL.filter (\(x,y,_,_) -> x == (currentx + 1) && y == currenty) b
                          | otherwise
                          -> 2 
              when (z5 /= oldC) $
                setSpanRight p False 
            scanWhileX b rst p oldC newC ffst (w,h) (currentx,currenty + 1)

{--------------------------------------}
