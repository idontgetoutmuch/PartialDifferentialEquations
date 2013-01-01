{-# LANGUAGE FlexibleContexts, TypeOperators, ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-type-defaults #-}
    
import Data.Array.Repa as Repa
import Control.Monad

r, sigma, k, t, xMax, deltaX, deltaT :: Double
m, n, p :: Int
r = 0.05
sigma = 0.2
k = 50.0
t = 3.0
m = 80
p = 1000
xMax = 150
deltaX = xMax / (fromIntegral m)
n = 800
deltaT = t / (fromIntegral n)

singleUpdater a = traverse a id f
  where
    Z :. m = extent a
    f _get (Z :. ix) | ix == 0   = 0.0
    f _get (Z :. ix) | ix == m-1 = xMax - k
    f  get (Z :. ix)             = a * get (Z :. ix-1) +
                                   b * get (Z :. ix) +
                                   c * get (Z :. ix+1)
      where
        a = deltaT * (sigma^2 * (fromIntegral ix)^2 - r * (fromIntegral ix)) / 2
        b = 1 - deltaT * (r  + sigma^2 * (fromIntegral ix)^2)
        c = deltaT * (sigma^2 * (fromIntegral ix)^2 + r * (fromIntegral ix)) / 2

priceAtT :: Array U DIM1 Double
priceAtT = fromListUnboxed (Z :. m+1) [max 0 (deltaX * (fromIntegral j) - k) | j <- [0..m]]

testSingle :: IO (Array U DIM1 Double)
testSingle = computeP $ singleUpdater priceAtT 

multiUpdater a = fromFunction (extent a) f
     where
       f :: DIM2 -> Double
       f (Z :. ix :. jx) = (singleUpdater x)!(Z :. ix)
         where
           x :: Array D DIM1 Double
           x = slice a (Any :. jx)

priceAtTMulti :: Array U DIM2 Double
priceAtTMulti = fromListUnboxed (Z :. m+1 :. p+1)
                [max 0 (deltaX * (fromIntegral j) - (k + (fromIntegral l)/1000.0)) | j <- [0..m], l <- [0..p]]

testMulti :: IO (Array U DIM2 Double)
testMulti = computeP $ multiUpdater priceAtTMulti

testMulti' :: IO (Array U DIM2 Double)
testMulti' = -- computeP $ updater (delay priceAtTMulti)
             updaterM priceAtTMulti
  where
    updater = foldr (.) id (replicate 800 multiUpdater)
    updaterM :: Monad m => Array U DIM2 Double -> m (Array U DIM2 Double)
    updaterM = foldr (>=>) return (replicate 15 (computeP . multiUpdater))

pickAtStrike :: Monad m => Array U DIM2 Double -> m (Array U DIM1 Double)
pickAtStrike t = computeP $ slice t (Any :. (27 :: Int) :. All)

main = do t <- testMulti'
          vStrikes <- pickAtStrike t
          putStrLn $ show vStrikes