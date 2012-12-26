-- @MISC{Zvan98discreteasian,
--     author = {R. Zvan and P. A. Forsyth and K.R. VETZAL and Peter A. Forsyth},
--     title = {Discrete Asian Barrier Options},
--     year = {1998}
-- }

{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-type-defaults #-}

import Data.Array.Repa as Repa
import Data.Array.Repa.Eval
import Control.Monad

r, sigma, k, t, xMax, deltaX, deltaT :: Double
m, n :: Int
r = 0.05
sigma = 0.2
k = 50.0
t = 3.0
m = 80
xMax = 150
deltaX = xMax / (fromIntegral m)
n = 800
deltaT = t / (fromIntegral n)

data PointedArrayU a = PointedArrayU Int (Array U DIM1 a)
  deriving Show

f :: PointedArrayU Double -> Double
f (PointedArrayU j _x) | j == 0 = 0.0
f (PointedArrayU j _x) | j == m = xMax - k
f (PointedArrayU j  x)          = a * x! (Z :. j-1) +
                                  b * x! (Z :. j) +
                                  c * x! (Z :. j+1)
  where
    a = deltaT * (sigma^2 * (fromIntegral j)^2 - r * (fromIntegral j)) / 2
    b = 1 - deltaT * (r  + sigma^2 * (fromIntegral j)^2)
    c = deltaT * (sigma^2 * (fromIntegral j)^2 + r * (fromIntegral j)) / 2

priceAtT :: PointedArrayU Double
priceAtT = PointedArrayU 0 (fromListUnboxed (Z :. m+1) 
                           [ max 0 (deltaX * (fromIntegral j) - k) | j <- [0..m] ])

coBindU :: (Source U a, Source U b, Target U b, Monad m) =>
           PointedArrayU a -> (PointedArrayU a -> b) -> m (PointedArrayU  b)
coBindU (PointedArrayU i a) f = computeP newArr >>= return . PointedArrayU i
  where
      newArr = traverse a id g
        where
          g _get (Z :. j) = f $ PointedArrayU j a

testN :: Int -> IO (PointedArrayU Double)
testN n =  h priceAtT
           where
           h = foldr (>=>) return
               (take n $ Prelude.zipWith flip (repeat coBindU) (repeat f))

-- So far so good but this has not bought us very much over using
-- Data.Array or Data.Vector.
--
-- 
