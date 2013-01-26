{-# LANGUAGE FlexibleContexts #-}

import Text.Printf

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine

import Diagrams.TwoD.Text

type MyDiagram = Diagram SVG R2

tickSize :: Double
tickSize = 0.1

gridLineWidth :: Double
gridLineWidth = 0.001

background :: MyDiagram
background = rect 1.2 1.2 # translate (r2 (0.5, 0.5))

values :: [(Double, Double)] -> [Double] -> MyDiagram
values xys vs = mconcat $ zipWith tick zs vs
  where
    xs = map fst xys
    ys = map snd xys
    zs = [ (x, y) | x <- xs, y <- ys]
    -- FIXME: This is clearly wrong here and may not even have been
    -- right where it was used originally.
    maxX  = maximum xs
    tSize = maxX / 100

    tick (x, y) v = endpt # translate tickShift
      where
        tickShift = r2 (x, y)
        endpt     = myText (printf "%.2f" v) # fontSize (tSize * 2) <>
                    circle tSize # fc blue # lw 0
        myText = alignedText 0.0 0.0

ticks :: [Double] -> MyDiagram
ticks xs = (mconcat $ Prelude.map tick xs)  <> line
  where
    maxX   = maximum xs
    line   = fromOffsets [r2 (maxX, 0)] # lw gridLineWidth
    tSize  = maxX / 100
    tick x = endpt # translate tickShift
      where
        tickShift = r2 (x, 0)
        endpt     = topLeftText (printf "%.2f" x) # fontSize (tSize * 2) <>
                    circle tSize # fc red # lw 0

ticksY :: [Double] -> MyDiagram
ticksY xs = (mconcat $ Prelude.map tick xs)  <> line
  where
    maxX   = maximum xs
    line   = fromOffsets [r2 (0, maxX)] # lw gridLineWidth
    tSize  = maxX / 100
    tick x = endpt # translate tickShift
      where
        tickShift = r2 (0, x)
        endpt     = myText (printf "%.2f" x) # fontSize (tSize * 2) <>
                    circle tSize # fc red # lw 0
        myText = alignedText 1.0 0.5

grid :: [Double] -> MyDiagram
grid xs = mconcat lines <> mconcat lineYs
  where
    maxX   = maximum xs
    lines = Prelude.map line xs
    lineYs = Prelude.map lineY xs
    line x  = fromOffsets [r2 (x, 0), r2 (0, maxX)] # lw gridLineWidth
    lineY y = fromOffsets [r2 (0, y), r2 (maxX, 0)] # lw gridLineWidth

main :: IO ()
main = do let xs = [0.0, tickSize..1.0]
              ys = [0.0, tickSize..1.0]
              zs = [0.0, tickSize..1.0]
          defaultMain $ values (zip xs ys) ([1,2..]) <>
                        ticks  xs <>
                        ticksY ys <>
                        grid zs <>
                        background
