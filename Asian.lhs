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

Our usual imports and options.

\begin{code}
{-# LANGUAGE FlexibleContexts, TypeOperators, NoMonomorphismRestriction #-}

{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-type-defaults #-}

import Data.Array.Repa as Repa hiding ((++))
import Data.Array.Repa.Repr.Unboxed as RepaU
import Control.Monad
import AsianDiagram
import Diagrams.Prelude((===))
import Diagrams.Backend.Cairo.CmdLine
import Text.Printf
import Data.List

\end{code}

First some constants for the payoff and the pricer. We make the space
grid deliberately coarse so we can draw diagrams showing the grid
before and after interfacing.

\begin{code}
r, sigma, k, t, xMax, aMax, deltaX, deltaT, deltaA:: Double
m, n, p :: Int
r = 0.05
sigma = 0.2
k = 50.0
t = 3.0
m = 10
p = 10
xMax = 150
deltaX = xMax / (fromIntegral m)
aMax = 150
deltaA = aMax / (fromIntegral p)
n = 800
deltaT = t / (fromIntegral n)
\end{code}

As before we can define a single pricer that updates the grid over one
time step and at multiple points in space using the Explicit Euler
method. We parameterize the upper and lower boundaries $z(0,t)$ and
$\lim_{x \to \infty}z(x,t)$ as we will want to use our pricer not just
for a vanilla call option.

\begin{code}
type BoundaryCondition = Array D DIM1 Double -> Double

singleUpdater :: BoundaryCondition ->
                  BoundaryCondition ->
                  Array D DIM1 Double ->
                  Array D DIM1 Double
singleUpdater lb ub arr = traverse arr id f
  where
    Z :. m = extent arr
    f _get (Z :. ix) | ix == 0   = lb arr
    f _get (Z :. ix) | ix == m-1 = ub arr
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
                BoundaryCondition ->
                BoundaryCondition ->
                Array r DIM2 Double ->
                Array D DIM2 Double
multiUpdater lb ub arr = fromFunction (extent arr) f
     where
       f :: DIM2 -> Double
       f (Z :. ix :. jx) = (singleUpdater lb ub x)!(Z :. ix)
         where
           x :: Array D DIM1 Double
           x = slice arr (Any :. jx)
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
\end{code}

Now instead of stepping all the way back to the initial date of the
option, we only step back as far as the last Asian time.

\begin{code}
asianTimes :: [Int]
asianTimes = Prelude.map (\x -> floor $ x*(fromIntegral n)/t) [2.7,2.8,2.9]

stepMulti :: Int ->
              BoundaryCondition ->
              BoundaryCondition ->
              Array U DIM2 Double ->
              IO (Array U DIM2 Double)
stepMulti n lb ub = updaterM
  where
    updaterM :: Monad m => Array U DIM2 Double -> m (Array U DIM2 Double)
    updaterM = foldr (>=>) return (replicate n (computeP . multiUpdater lb ub))
\end{code}

But now we are stuck. What are the boundary conditions for each of the
pricers after we have done our interfacing? We follow many
practioners\cite{Windcliff03analysisof,2012arXiv1208.5168I} and assume
that the underlying is linear in this area. Thus in the Black-Scholes
equation

$$
\frac{\partial z}{\partial t} + \half \sigma^2 x^2 \frac{\partial^2 z}{\partial x^2} + r x\frac{\partial z}{\partial x} - rz = 0
$$

we set the second derivative to $0$. Thus we can use a forward
method at the boundary to solve the equation below.

$$
\frac{\partial z}{\partial t} + r x\frac{\partial z}{\partial x} - rz = 0
$$

When $x=0$, we have a simpler situation.

$$
\frac{\partial z(0,t)}{\partial t} - rz = 0
$$

The corresponding difference equation for the lower boundary is:

$$
\frac{z_0^{n+1} - z_0^n}{\Delta t} -rz_0^n = 0
$$

Which is easily represented in Haskell (rembering we are stepping
backwards in time).

\begin{code}
lBoundaryUpdater :: BoundaryCondition
lBoundaryUpdater arr = x - deltaT * r * x
  where
    x = arr!(Z :. (0 :: Int))
\end{code}

The corresponding difference equation for the upper boundary is:

\begin{align*}
\frac{z^{n+1}_m - z^n_m}{\Delta t} +
r(m\Delta x)\frac{z^n_m - z^n_{m-1}}{\Delta x} -
rz^n_m = 0
\end{align*}

Re-arranging

\begin{align*}
\frac{z^{n+1}_m - z^n_m}{\Delta t} +
r (m-1) z^n_m -
r m z^n_{m-1} = 0
\end{align*}

We can write this in Haskell as follows (again remembering we are
stepping backwards in time).

\begin{code}
uBoundaryUpdater :: BoundaryCondition
uBoundaryUpdater arr = x + deltaT * r * (a - b)
  where
    Z :. m = extent arr
    x = arr!(Z :. m - 1)
    y = arr!(Z :. m - 2)
    a = x * fromIntegral (m-1)
    b = y * fromIntegral m
\end{code}

Now we can step backwards in time to just before the last observation.

\begin{code}
justBefore3 :: IO (Array U DIM2 Double)
justBefore3 = stepMulti (n - (asianTimes!!2) -1) lBoundaryUpdater uBoundaryUpdater priceAtTAsian
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
justBefore2 :: IO (Array U DIM2 Double)
justBefore2 = stepMulti undefined undefined undefined undefined
\end{code}

\begin{code}
pickAtStrike :: Monad m => Int -> Array U DIM2 Double -> m (Array U DIM1 Double)
pickAtStrike n t = computeP $ slice t (Any :. n :. All)

showD :: Double -> String
showD = printf "%.2f"

showArrD1 :: Array U DIM1 Double -> String
showArrD1 = intercalate ", " . Prelude.map showD . toList


main :: IO ()
main = do -- t <- stepMulti n priceAtTAsian
          -- vStrikes <- pickAtStrike 27 t
          -- putStrLn $ show vStrikes
          let (Z :. _i :. j) = extent priceAtTAsian

          putStrLn "\nAsianing times"
          putStrLn $ show asianTimes

          putStrLn "\nInitial pricers"
          let aSlicesD'' = Prelude.map (\m -> slice priceAtTAsian (Any :. m )) [0..j-1]

          aSlices'' <- mapM computeP aSlicesD'' :: IO [Array U DIM1 Double]
          mapM_ (putStrLn . showArrD1) aSlices''

          grida <- justBefore3
          let aSlicesDa :: [Array D DIM1 Double]
              aSlicesDa = Prelude.map (\m -> slice grida (Any :. m)) [0..j-1]
          aSlicesa <- mapM computeP aSlicesDa :: IO [Array U DIM1 Double]

          putStrLn "\nJust before 3"
          putStrLn $ show $ Prelude.map extent aSlicesa
          mapM_ (putStrLn . showArrD1) aSlicesa

          grid' <- computeP $ interface grida :: IO (Array U DIM2 Double)

          defaultMain $     drawValues grida
                        === drawValues grid'

          let aSlicesD' :: [Array D DIM1 Double]
              aSlicesD' = Prelude.map (\m -> slice grid' (Any :. m)) [0..j-1]
          aSlices' <- mapM computeP aSlicesD' :: IO [Array U DIM1 Double]

          putStrLn "\nJust after 3"
          putStrLn $ show $ Prelude.map extent aSlices'
          mapM_ (putStrLn . showArrD1) aSlices'

          putStrLn "\nDiagonal"
          putStrLn $ intercalate ", " $ Prelude.map showD $ Prelude.zipWith ((!!)) (Prelude.map toList aSlices') [0,1..p]

\end{code}

\end{document}
