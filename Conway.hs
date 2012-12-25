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
m = 3
xMax = 150
deltaX = xMax / (fromIntegral m)
n = 3
deltaT = t / (fromIntegral n)

data PointedArray a = PointedArray Int (Array U DIM1 a)
  deriving Show

f :: PointedArray Double -> Double
f (PointedArray j _x) | j == 0 = 0.0
f (PointedArray j _x) | j == m = xMax - k
f (PointedArray j  x)          = a * x!(Z :. j-1) + b * x!(Z :. j) + c * x!(Z :. j+1)
  where
    a = deltaT * (sigma^2 * (fromIntegral j)^2 - r * (fromIntegral j)) / 2
    b = 1 - deltaT * (r  + sigma^2 * (fromIntegral j)^2)
    c = deltaT * (sigma^2 * (fromIntegral j)^2 + r * (fromIntegral j)) / 2

priceAtT :: PointedArray Double
priceAtT = PointedArray 0 (fromListUnboxed (Z :. m+1) 
                           [ max 0 (deltaX * (fromIntegral j) - k) | j <- [0..m] ])

coBindU :: (Source U a, Source U b, Target U b, Monad m) =>
           PointedArray a -> (PointedArray a -> b) -> m (PointedArray  b)
coBindU (PointedArray i a) f = computeP newArr >>= return . PointedArray i
  where
      newArr = Repa.traverse a id g
        where
          g _get (Z :. j) = f $ PointedArray j a

testN :: Int -> IO (PointedArray Double)
testN n =  h priceAtT
           where
           h = foldr (>=>) return
               (take n $ Prelude.zipWith flip (repeat coBindU) (repeat f))
