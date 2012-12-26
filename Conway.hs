{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-type-defaults #-}

import Data.Array.Repa as R
import Data.Array.Repa.Eval
import Control.Monad

import Data.Array as A

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

class Comonad c where
  coreturn :: c a -> a
  (=>>) :: c a -> (c a -> b) -> c b

data PointedArray i a = PointedArray i (A.Array i a)
  deriving Show

instance Ix i => Functor (PointedArray i) where
  fmap f (PointedArray i a) = PointedArray i (fmap f a) 

instance Ix i => Comonad (PointedArray i) where
  coreturn (PointedArray i a) = a A.! i
  (PointedArray i a) =>> f =
    PointedArray i (listArray (bounds a) 
                   (Prelude.map (f . flip PointedArray a) (range $ bounds a)))

fArr :: PointedArray Int Double -> Double
fArr (PointedArray j _x) | j == 0 = 0.0
fArr (PointedArray j _x) | j == m = xMax - k
fArr (PointedArray j  x)          = a * x A.! (j-1) +
                                    b * x A.! j +
                                    c * x A.! (j+1)
  where
    a = deltaT * (sigma^2 * (fromIntegral j)^2 - r * (fromIntegral j)) / 2
    b = 1 - deltaT * (r  + sigma^2 * (fromIntegral j)^2)
    c = deltaT * (sigma^2 * (fromIntegral j)^2 + r * (fromIntegral j)) / 2

priceAtTArr :: PointedArray Int Double
priceAtTArr = PointedArray 0 (listArray (0, m)
                              [ max 0 (deltaX * (fromIntegral j) - k) | j <- [0..m] ])

prices :: [PointedArray Int Double]
prices = iterate (=>> fArr) priceAtTArr

pricesAtM :: Ix i => i -> PointedArray i e -> e
pricesAtM m (PointedArray _ x) = x A.! m

price :: Int -> Double
price m = last $ Prelude.map (pricesAtM m) $ take n $ iterate (=>> fArr) priceAtTArr

data PointedArrayU a = PointedArrayU Int (R.Array U DIM1 a)
  deriving Show

f :: PointedArrayU Double -> Double
f (PointedArrayU j _x) | j == 0 = 0.0
f (PointedArrayU j _x) | j == m = xMax - k
f (PointedArrayU j  x)          = a * x R.! (Z :. j-1) +
                                  b * x R.! (Z :. j) +
                                  c * x R.! (Z :. j+1)
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
      newArr = R.traverse a id g
        where
          g _get (Z :. j) = f $ PointedArrayU j a

testN :: Int -> IO (PointedArrayU Double)
testN n =  h priceAtT
           where
           h = foldr (>=>) return
               (take n $ Prelude.zipWith flip (repeat coBindU) (repeat f))

