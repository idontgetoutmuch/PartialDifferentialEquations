\documentclass[12pt]{article}
%include polycode.fmt
\usepackage[pdftex,pagebackref,letterpaper=true,colorlinks=true,pdfpagemode=none,urlcolor=blue,linkcolor=blue,citecolor=blue,pdfstartview=FitH]{hyperref}

\usepackage{amsmath,amsfonts}
\usepackage{graphicx}
\usepackage{color}

\setlength{\oddsidemargin}{0pt}
\setlength{\evensidemargin}{0pt}
\setlength{\textwidth}{6.0in}
\setlength{\topmargin}{0in}
\setlength{\textheight}{8.5in}

\setlength{\parindent}{0in}
\setlength{\parskip}{5px}

\newcommand{\half}{\frac{1}{2}}
\newcommand{\cobind}{\mathbin{=\mkern-6.7mu>\!\!\!>}}

%format =>> = "\cobind "

\begin{document}

Can we get better performance for pricing financial options?

First let us translate our pricer using the
\href{http://idontgetoutmuch.wordpress.com/2012/04/01/solving-a-partial-differential-equation-comonadically/}{Explicit
Euler Method} to use \href{http://repa.ouroborus.net}{repa}.

\begin{code}
{-# LANGUAGE FlexibleContexts, TypeOperators #-}

{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-type-defaults #-}
    
import Data.Array.Repa as Repa
import Data.Array.Repa.Eval
import Control.Monad

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

data PointedArrayU a = PointedArrayU Int (Array U DIM1 a)
  deriving Show

singleUpdaterP :: PointedArrayU Double -> Double
singleUpdaterP (PointedArrayU j _x) | j == 0 = 0.0
singleUpdaterP (PointedArrayU j _x) | j == m = xMax - k
singleUpdaterP (PointedArrayU j  x)          = a * x! (Z :. j-1) +
                                               b * x! (Z :. j) +
                                               c * x! (Z :. j+1)
  where
    a = deltaT * (sigma^2 * (fromIntegral j)^2 - r * (fromIntegral j)) / 2
    b = 1 - deltaT * (r  + sigma^2 * (fromIntegral j)^2)
    c = deltaT * (sigma^2 * (fromIntegral j)^2 + r * (fromIntegral j)) / 2

priceAtTP :: PointedArrayU Double
priceAtTP = PointedArrayU 0 (fromListUnboxed (Z :. m+1) 
                            [ max 0 (deltaX * (fromIntegral j) - k) | j <- [0..m] ])

coBindU :: (Source U a, Source U b, Target U b, Monad m) =>
           PointedArrayU a -> (PointedArrayU a -> b) -> m (PointedArrayU  b)
coBindU (PointedArrayU i a) f = computeP newArr >>= return . PointedArrayU i
  where
      newArr = traverse a id g
        where
          g _get (Z :. j) = f $ PointedArrayU j a

testN :: Int -> IO (PointedArrayU Double)
testN n =  h priceAtTP
           where
           h = foldr (>=>) return
               (take n $ Prelude.zipWith flip (repeat coBindU) (repeat singleUpdaterP))
\end{code}

So far so good but this has not bought us very much over using {\em
Data.Array} or {\em Data.Vector}. Morevoer we have not been able to
use the costate comonad because of the restrictions on types imposed
by {\em repa}.

Let us re-write the above without even attempting to mimic the cobind
operator. Thus we no longer need our pointed array.

\begin{code}
singleUpdater :: Array D DIM1 Double -> Array D DIM1 Double
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
\end{code}

Now suppose we want to price a spot ladder then we would like to apply the same update function to slightly modified payoffs (the terminal boundary condition). First let's utilise our single update function to work over two dimensional grid.

\begin{code}
multiUpdater :: Source r Double => Array r DIM2 Double -> Array D DIM2 Double
multiUpdater a = fromFunction (extent a) f
     where
       f :: DIM2 -> Double
       f (Z :. ix :. jx) = (singleUpdater x)!(Z :. ix)
         where
           x :: Array D DIM1 Double
           x = slice a (Any :. jx)
\end{code}

We define our spot ladder on a call to be many calls each with a
strike differing by 10 basis points (a basis point is one hundredth of
one percent).

\begin{code}
priceAtTMulti :: Array U DIM2 Double
priceAtTMulti = fromListUnboxed (Z :. m+1 :. p+1)
                [ max 0 (deltaX * (fromIntegral j) - (k + (fromIntegral l)/1000.0))
                | j <- [0..m]
                , l <- [0..p]
                ]
\end{code}

Now we can test our spot ladder pricer. Note that we force the
evaluation of our two dimensional array at each time step. If we do
not do that then we end up with a leak as presumably {\em repa} tries to
fuse many updates. Moreover fusing updates across time steps does not
really buy us anything.

\begin{code}
testMulti :: IO (Array U DIM2 Double)
testMulti = updaterM priceAtTMulti
  where
    updaterM :: Monad m => Array U DIM2 Double -> m (Array U DIM2 Double)
    updaterM = foldr (>=>) return (replicate n (computeP . multiUpdater))

pickAtStrike :: Monad m => Int -> Array U DIM2 Double -> m (Array U DIM1 Double)
pickAtStrike n t = computeP $ slice t (Any :. n :. All)

main :: IO ()
main = do t <- testMulti
          vStrikes <- pickAtStrike 27 t
          putStrLn $ show vStrikes
\end{code}

\end{document}
