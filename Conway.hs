{-# LANGUAGE TemplateHaskell, QuasiQuotes, NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

import qualified Data.Vector.Unboxed as Unboxed
import Data.Array.Repa as Repa
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import Data.Array.Repa.Eval

r = 0.05
sigma = 0.2
k = 50.0
t = 3.0
m = 3
xMax = 150
deltaX = xMax / (fromIntegral m)
n = 3
deltaT = t / (fromIntegral n)

data PointedArray t a = PointedArray Int (Array t DIM1 a)

f :: Source r Double => PointedArray r Double -> Double
f (PointedArray j x) | j == 0 = 0.0
f (PointedArray j x) | j == m = xMax - k
f (PointedArray j x)          = a * x!(Z :. j-1) + b * x!(Z :. j) + c * x!(Z :. j+1)
  where
    a = deltaT * (sigma^2 * (fromIntegral j)^2 - r * (fromIntegral j)) / 2
    b = 1 - deltaT * (r  + sigma^2 * (fromIntegral j)^2)
    c = deltaT * (sigma^2 * (fromIntegral j)^2 + r * (fromIntegral j)) / 2

priceAtT :: PointedArray U Double
priceAtT = PointedArray 0 (fromListUnboxed (Z :. m+1) 
                           [ max 0 (deltaX * (fromIntegral j) - k) | j <- [0..m] ])

coBind :: (Source D a, Source D b, Target D b, Monad m) =>
          PointedArray D a -> (PointedArray D a -> b) -> m (PointedArray D b)
coBind (PointedArray i a) f = computeP newArr >>= return . PointedArray i
  where
      newArr = Repa.traverse a id g
        where
          g get (Z :. j) = f $ PointedArray j a

-- test1 :: IO String
-- test1 = do PointedArray _ a <- test
--            return $ show a
--              where
--                test :: IO (PointedArray U Double)
--                test = coBind priceAtT f 

-- test2 :: IO String        
-- test2 = do undefined
--              where
--                test :: IO (PointedArray U Double)
--                test = do x <- coBind priceAtT f
--                          return $ coBind x f

-- ((((flip coBind f) priceAtT :: IO (PointedArray U Double)) >>= flip coBind f) :: IO (PointedArray U Double)) >>= return . show . (\(PointedArray i a) -> a)
