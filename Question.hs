{-# LANGUAGE FlexibleContexts, TypeOperators, ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-type-defaults #-}
    
import Data.Array.Repa as Repa
import Control.Monad
import qualified Data.Array as A

r, sigma, k, t, xMax, deltaX, deltaT :: Double
m, n, p :: Int
r = 0.05
sigma = 0.2
k = 50.0
t = 3.0
m = 80
p = 99
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
                [ max 0 (deltaX * (fromIntegral j) - (k + (fromIntegral l)/1000.0))
                | j <- [0..m]
                , l <- [0..p]
                ]

testMulti :: IO (Array U DIM2 Double)
testMulti = computeP $ multiUpdater priceAtTMulti

testMulti' :: IO (Array U DIM2 Double)
testMulti' = updaterM priceAtTMulti
  where
    updaterM :: Monad m => Array U DIM2 Double -> m (Array U DIM2 Double)
    updaterM = foldr (>=>) return (replicate n (computeP . multiUpdater))

pickAtStrike :: Monad m => Array U DIM2 Double -> m (Array U DIM1 Double)
pickAtStrike t = computeP $ slice t (Any :. (25 :: Int) :. All)

main :: IO ()
main = do t <- testMulti'
          vStrikes <- pickAtStrike t
          -- putStrLn $ show t
          putStrLn $ show vStrikes

-- main = do putStrLn $ show $ price' 25

class Comonad c where
  coreturn :: c a -> a
  (=>>) :: c a -> (c a -> b) -> c b

data PointedArray i a = PointedArray i (A.Array i a)
  deriving Show

instance A.Ix i => Functor (PointedArray i) where
  fmap f (PointedArray i a) = PointedArray i (fmap f a) 

instance A.Ix i => Comonad (PointedArray i) where
  coreturn (PointedArray i a) = a A.! i
  (PointedArray i a) =>> f =
    PointedArray i (A.listArray (A.bounds a) 
                   (Prelude.map (f . flip PointedArray a) (A.range $ A.bounds a)))

f (PointedArray j _x) | j == 0 = 0.0
f (PointedArray j _x) | j == m = xMax - k
f (PointedArray j  x)          = a * x A.! (j-1) + b * x A.! j + c * x A.! (j+1)
  where
    a = deltaT * (sigma^2 * (fromIntegral j)^2 - r * (fromIntegral j)) / 2
    b = 1 - deltaT * (r  + sigma^2 * (fromIntegral j)^2)
    c = deltaT * (sigma^2 * (fromIntegral j)^2 + r * (fromIntegral j)) / 2

priceAtTA :: PointedArray Int Double
priceAtTA = PointedArray 0 (A.listArray (0, m)
                            [ max 0 (deltaX * (fromIntegral j) - k) | j <- [0..m] ])

priceAtTAs :: [PointedArray Int Double]
priceAtTAs = Prelude.map (f . (/1000.0)) [1.0,2.0..100.0]
  where
    f x =  PointedArray 0 (A.listArray (0, m)
                          [ max 0 (deltaX * (fromIntegral j) - k + x) | j <- [0..m] ])

prices = iterate (=>> f) priceAtTA
pricess = Prelude.map (iterate (=>> f)) priceAtTAs

pricesAtM m (PointedArray _ x) = x A.! m
price m = last $ Prelude.map (pricesAtM m) $ take n $ iterate (=>> f) priceAtTA
price' m = Prelude.map (\priceAtTA -> last $ Prelude.map (pricesAtM m) $ take n $ iterate (=>> f) priceAtTA) priceAtTAs
