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

Now we are able to price options using parallelism, let us consider a
more exotic financial option.

Let us suppose that we wish to price an Asian call
option\cite{Zvan98discreteasian}. The payoff at time $T$ is

$$
{\Bigg(\frac{\Sigma_{i=1}^n x_{t_i}}{n} -k\Bigg)}^+
$$

Thus the payoff depends not just on the value of the underlying $x$ at
time $T$ but also on the path taken. We do not need to know the entire
path, just the average at discrete points in time. Thus the payoff is
a function of time, the value of the underlying and the average of the
underlying sampled at discrete points in time $z(x,a,t)$.

We can rewrite this equation as a recursive relation:

$$
a_n = a_{n-1} + \frac{x_n -a_{n-1}}{n}
$$

If $t_n$ is the $n$-th sampling time then for small $\epsilon$ let:

\begin{align*}
t^+ &= t_n + \epsilon \\
t^- &= t_n - \epsilon
\end{align*}

Now we can write the equation for the value of the average just before
and after the sampling time as:

$$
a(x, t^+) = a(x, t^-) + \frac{x - a(x, t^-)}{n}
$$

Since there is no arbitrage, the value of the option cannot change as
it crosses the sampling date:

$$
z(x, a^+, t^+) = z(x, a^-, t^-)
$$

Substituting in:

$$
z(x, a + (x - a)/n, t^+) = z(x, a, t^-)
$$

But we have a problem, we do not know the value of $a$. Away from the
sampling times there is no dependence on $a$ as its value cannot
change.  We already know how to step back in time in this case: we use
the pricer we have already developed.

So let us assume a value for $a$. Then we can diffuse backwards from
the final payoff but when we reach a sampling date, we reset the
values on the grid using the interfacing formula above.

In summary, we assume a set of values for $a$ and then diffuse
backwards for each pricer with that $a$; at each sampling time, we
reset the values of the pricers and continue until we reach the last
sampling time. At this point, $x = a$ so we diffuse one pricer back to
the start time which gives us the price of the option for any given
$x$ on our grid.

\begin{code}
{-# LANGUAGE FlexibleContexts, TypeOperators #-}

{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-type-defaults #-}

import Data.Array.Repa as Repa hiding ((++))
import Data.Array.Repa.Repr.Unboxed as RepaU
import Control.Monad

-- import Text.Printf

-- import Diagrams.Prelude ((<>), lw, (#), red, fc, circle, fontSize, r2,
--                          mconcat, translate, rect, fromOffsets, topLeftText,
--                          alignedText)
-- import Diagrams.Backend.Cairo.CmdLine
\end{code}

First some constants for the payoff and the pricer.

\begin{code}
r, sigma, k, t, xMax, aMax, deltaX, deltaT, deltaA:: Double
m, n, p :: Int
r = 0.00 -- 0.05
sigma = 0.2
k = 50.0
t = 3.0
m = 6 -- 80
p = 6 -- 10
xMax = 150
deltaX = xMax / (fromIntegral m)
aMax = 150
deltaA = aMax / (fromIntegral p)
n = 800
deltaT = t / (fromIntegral n)
\end{code}

We need some constants to make sure our diagrams render correctly.

\begin{code}
tickSize :: Double
tickSize = 0.1

lineWidth :: Double
lineWidth = 0.001
\end{code}

As before we can define a single pricer that updates the grid over one
time step and at multiple points in space using the Explicit Euler
method. We parameterize the upper and lower boundaries $z(0,t)$ and
$\lim_{x \to \infty}z(x,t)$ as we will want to use our pricer not just
for a vanilla call option.

\begin{code}
singleUpdater :: Double -> Double -> Array D DIM1 Double -> Array D DIM1 Double
singleUpdater lb ub a = traverse a id f
  where
    Z :. m = extent a
    f _get (Z :. ix) | ix == 0   = lb
    f _get (Z :. ix) | ix == m-1 = ub
    f  get (Z :. ix)             = a * get (Z :. ix-1) +
                                   b * get (Z :. ix) +
                                   c * get (Z :. ix+1)
      where
        a = deltaT * (sigma^2 * (fromIntegral ix)^2 - r * (fromIntegral ix)) / 2
        b = 1 - deltaT * (r  + sigma^2 * (fromIntegral ix)^2)
        c = deltaT * (sigma^2 * (fromIntegral ix)^2 + r * (fromIntegral ix)) / 2
\end{code}

Again we can extend this to update many pricers on one time step and
multiple points in space.

\begin{code}
multiUpdater :: Source r Double =>
                Array U DIM1 (Double, Double) ->
                Array r DIM2 Double ->
                Array D DIM2 Double
multiUpdater lubs a = fromFunction (extent a) f
     where
       f :: DIM2 -> Double
       f (Z :. ix :. jx) = (singleUpdater lb ub x)!(Z :. ix)
         where
           (lb, ub) = lubs!(Z :. jx)
           x :: Array D DIM1 Double
           x = slice a (Any :. jx)
\end{code}

We define the boundary condition at the maturity date of our Asian
option. Notice for each pricer the boundary condition is constant,
this being determined by the value of the Asian payoff.

\begin{code}
priceAtTAsian :: Array U DIM2 Double
priceAtTAsian = fromListUnboxed (Z :. m+1 :. p+1)
                [ max 0 (deltaA * (fromIntegral l) - k)
                | _j <- [0..m],
                   l <- [0..p]
                ]

asianBCs :: Array U DIM1 (Double, Double)
asianBCs = RepaU.zip lbs ubs
             where
               lbs = computeS $ slice priceAtTAsian (Any :. (0 ::Int) :. All)
               ubs = computeS $ slice priceAtTAsian (Any :. m :. All)
\end{code}

Now instead of stepping all the way back to the initial date of the
option, we only step back as far as the last Asian time.

\begin{code}
asianTimes :: [Int]
asianTimes = Prelude.map (\x -> floor $ x*(fromIntegral n)/t) [2.7,2.8,2.9]

testMulti :: Int -> Array U DIM1 (Double, Double) ->
             Array U DIM2 Double ->
             IO (Array U DIM2 Double)
testMulti n lubs = updaterM
  where
    updaterM :: Monad m => Array U DIM2 Double -> m (Array U DIM2 Double)
    updaterM = foldr (>=>) return (replicate n (computeP . multiUpdater lubs))

justBefore3 :: IO (Array U DIM2 Double)
justBefore3 = testMulti (n - (asianTimes!!2) -1) asianBCs priceAtTAsian

justBefore2 :: IO (Array U DIM2 Double)
justBefore2 = testMulti undefined undefined undefined
\end{code}

Now we can do our interfacing.

\begin{code}
interface :: Array U DIM2 Double -> Array D DIM2 Double
interface grid = traverse grid id (\_ sh -> f sh)
  where
    (Z :. _iMax :. jMax) = extent grid
    f (Z :. i :. j) = inter
      where
        x       = deltaX * (fromIntegral i)
        aPlus   = deltaA * (fromIntegral j)
        aMinus  = aPlus + (x - aPlus) / (fromIntegral $ length asianTimes)
        jLower  = if k > jMax - 1 then jMax - 1 else k
                    where k = floor $ aMinus / deltaA
        jUpper  = if (jLower == jMax - 1) then jMax - 1 else jLower + 1
        aLower  = deltaA * (fromIntegral jLower)
        prptn   = (aMinus - aLower) / deltaA
        vLower  = grid!(Z :. i :. jLower)
        vUpper  = grid!(Z :. i :. jUpper)
        inter   = vLower + prptn * (vUpper - vLower)
\end{code}

\begin{code}
pickAtStrike :: Monad m => Int -> Array U DIM2 Double -> m (Array U DIM1 Double)
pickAtStrike n t = computeP $ slice t (Any :. n :. All)

-- background = rect 1.2 1.2 # translate (r2 (0.5, 0.5))

-- ticks xs = (mconcat $ Prelude.map tick xs)  <> line
--   where
--     maxX   = maximum xs
--     line   = fromOffsets [r2 (maxX, 0)] # lw lineWidth
--     tSize  = maxX / 100
--     tick x = endpt # translate tickShift
--       where
--         tickShift = r2 (x, 0)
--         endpt     = topLeftText (printf "%.2f" x) # fontSize (tSize * 2) <>
--                     circle tSize # fc red  # lw 0

-- ticksY xs = (mconcat $ Prelude.map tick xs)  <> line
--   where
--     maxX   = maximum xs
--     line   = fromOffsets [r2 (0, maxX)] # lw lineWidth
--     tSize  = maxX / 100
--     tick x = endpt # translate tickShift
--       where
--         tickShift = r2 (0, x)
--         endpt     = myText (printf "%.2f" x) # fontSize (tSize * 2) <>
--                     circle tSize # fc red  # lw 0
--         myText = alignedText 1.0 0.5

-- grid xs = mconcat lines <> mconcat lineYs
--   where
--     maxX   = maximum xs
--     lines = Prelude.map line xs
--     lineYs = Prelude.map lineY xs
--     line x  = fromOffsets [r2 (x, 0), r2 (0, maxX)] # lw lineWidth
--     lineY y = fromOffsets [r2 (0, y), r2 (maxX, 0)] # lw lineWidth

main :: IO ()
main = do -- t <- testMulti n priceAtTAsian
          -- vStrikes <- pickAtStrike 27 t
          -- putStrLn $ show vStrikes
          let (Z :. _i :. j) = extent priceAtTAsian

          let aSlicesD'' = Prelude.map (\m -> slice priceAtTAsian (Any :. m )) [0..j-1]

          aSlices'' <- mapM computeP aSlicesD'' :: IO [Array U DIM1 Double]
          mapM_ (putStrLn . show . toList) aSlices''

          putStrLn $ show asianTimes
          grid <- justBefore3
          let aSlicesD :: [Array D DIM1 Double]
              aSlicesD = Prelude.map (\m -> slice grid (Any :. m)) [0..j-1]
          aSlices <- mapM computeP aSlicesD :: IO [Array U DIM1 Double]
          mapM_ (putStrLn . show . toList) aSlices

          putStrLn $ show $ extent grid

          grid' <- computeP $ interface grid :: IO (Array U DIM2 Double)

          let aSlicesD' :: [Array D DIM1 Double]
              aSlicesD' = Prelude.map (\m -> slice grid' (Any :. m)) [0..j-1]
          aSlices' <- mapM computeP aSlicesD' :: IO [Array U DIM1 Double]
          mapM_ (putStrLn . show . toList) aSlices'
          putStrLn $ show $ Prelude.zipWith ((!!)) (Prelude.map toList aSlices') [0,1..p]

          -- putStrLn $ show $ extent priceAtTAsian
          -- defaultMain $ ticks  [0.0, tickSize..1.0] <>
          --               ticksY [0.0, tickSize..1.0] <>
          --               grid   [0.0, tickSize..1.0] <>
          --               background
\end{code}

\end{document}
