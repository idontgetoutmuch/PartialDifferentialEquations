{-# LANGUAGE TemplateHaskell, QuasiQuotes, NoMonomorphismRestriction #-}

import Data.Array.Repa hiding ((++))
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2

import Debug.Trace

sten :: Stencil DIM2 Int
sten = [stencil2| 1 1 1
                  1 0 1
                  1 1 1 |]

x1 :: Array U DIM2 Int
x1 = fromListUnboxed (Z :. (3 :: Int) :. (4 :: Int))
     [ 
       1, 2, 3, 1
     , 4, 9, 5, 2
     , 0, 0, 0, 3
     ]

conv1 :: Monad m => m (Array U DIM2 Int)
conv1 = computeP $ mapStencil2 (BoundConst 0) sten x1

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
f (PointedArray j x) | j == 0 = 0.0
f (PointedArray j x) | j == m = xMax - k
f (PointedArray j x)          = a * x!(Z :. j-1) + b * x!(Z :. j) + c * x!(Z :. j+1)
  where
    a = deltaT * (sigma^2 * (fromIntegral j)^2 - r * (fromIntegral j)) / 2
    b = 1 - deltaT * (r  + sigma^2 * (fromIntegral j)^2)
    c = deltaT * (sigma^2 * (fromIntegral j)^2 + r * (fromIntegral j)) / 2

priceAtT :: PointedArray Double
priceAtT = PointedArray 0 (fromListUnboxed (Z :. m+1) 
                           [ max 0 (deltaX * (fromIntegral j) - k) | j <- [0..m] ])


{-# INLINE solve #-}
solve arr
 = traverse arr id elemFn
 where  _ :. width :. height = extent arr
        {-# INLINE elemFn #-}
        elemFn get d@(Z :. i :. j)
          | j == 0      = trace ("1: " ++ show i ++ show j ++ ": 0.0") 0.0
          | j == m {- height -} = trace ("2: " ++ show i ++ show j ++ ": " ++ show (xMax - k)) xMax - k
          | i == 0      = trace ("3: " ++ show i ++ show j ++ ": " ++ show (max 0 (deltaX * fromIntegral j) - k)) max 0 (deltaX * (fromIntegral j) - k)
          | otherwise   = trace ("4: " ++ show i ++ show j ++ ": " ++ show foo) foo
          where
            foo =  a * (get (Z :. (i-1) :. (j-1))) +
                   b * (get (Z :. (i-1) :. j)) +
                   c * (get (Z :. (i-1) :. (j+1)))
            a = deltaT * (sigma^2 * (fromIntegral j)^2 - r * (fromIntegral j)) / 2
            b = 1 - deltaT * (r  + sigma^2 * (fromIntegral j)^2)
            c = deltaT * (sigma^2 * (fromIntegral j)^2 + r * (fromIntegral j)) / 2

initGrid :: Array U DIM2 Double
initGrid = fromListUnboxed (Z :. (m+1 :: Int) :. (n+1 :: Int)) (take ((n+1) * (m+1)) $ repeat 0.0)
           