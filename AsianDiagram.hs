{-# LANGUAGE FlexibleContexts, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-type-defaults #-}

module AsianDiagram (
    CoordinateValue (..)
  , CoordinateIx
  , drawValues
) where

import Text.Printf
import qualified Data.Map as Map
import Data.Map (Map)

import Diagrams.Prelude
import Diagrams.Backend.Cairo

type DiagramC = Diagram Cairo R2

gridLineWidth :: Double
gridLineWidth = 0.001

fSize :: Double
fSize = 0.02

cSize :: Double
cSize = 0.01

background :: DiagramC
background = rect 1.2 1.2 # translate (r2 (0.5, 0.5))

values :: [(Double, Double)] -> [Double] -> DiagramC
values xys vs = mconcat $ zipWith tick zs vs
  where
    xs = map fst xys
    ys = map snd xys
    zs = [ (x, y) | x <- xs, y <- ys]

    tick (x, y) v = endpt # translate tickShift
      where
        tickShift = r2 (x, y)
        endpt     = myText (printf "%.2f" v) # fontSize fSize <>
                    circle (cSize /2 ) # fc blue # opacity 0.5 # lw 0
        myText = alignedText 0.0 0.0

ticks :: [Double] -> DiagramC
ticks xs = (mconcat $ Prelude.map tick xs)  <> line
  where
    maxX   = maximum xs
    line   = fromOffsets [r2 (maxX, 0)] # lw gridLineWidth
    tick x = endpt # translate tickShift
      where
        tickShift = r2 (x, 0)
        endpt     = topLeftText (printf "%.2f" x) # fontSize fSize <>
                    circle cSize # fc red # lw 0

ticksY :: [Double] -> DiagramC
ticksY xs = (mconcat $ Prelude.map tick xs)  <> line
  where
    maxX   = maximum xs
    line   = fromOffsets [r2 (0, maxX)] # lw gridLineWidth
    tick x = endpt # translate tickShift
      where
        tickShift = r2 (0, x)
        endpt     = myText (printf "%.2f" x) # fontSize fSize <>
                    circle cSize # fc red # lw 0
        myText = alignedText 1.0 0.5

grid :: [Double] -> DiagramC
grid xs = mconcat lines <> mconcat lineYs
  where
    maxX   = maximum xs
    lines = Prelude.map line xs
    lineYs = Prelude.map lineY xs
    line x  = fromOffsets [r2 (x, 0), r2 (0, maxX)] # lw gridLineWidth
    lineY y = fromOffsets [r2 (0, y), r2 (maxX, 0)] # lw gridLineWidth

type CoordinateIx = (Int, Int)
data CoordinateValue = CoordinateValue {
                           xCoord :: Double
                         , yCoord :: Double
                         , gValue  :: Double
                         }

drawValues :: CoordinateIx `Map` CoordinateValue -> DiagramC
drawValues coordValMap =
       values (zip xs ys) zs
    <> ticks  xs
    <> ticksY ys
    <> grid xs
    <> background
  where
    xs = map xCoord $ Map.elems coordValMap
    ys = map yCoord $ Map.elems coordValMap
    zs = map gValue  $ Map.elems coordValMap
