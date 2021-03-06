-- | Main entry point to the application.
module Main where

import Debug.Trace

-- | ODE, y = e ^ x
mydydx1 :: Double -> Double -> Double
mydydx1 x y = do
    exp(x)

-- | ODE, y = x * e ^ (3 * x) - 2 * y
mydydx2 :: Double -> Double -> Double
mydydx2 x y = do
    x * exp (3 * x) - 2 * y

-- | ODE, y = r * y
mydydx3 :: Double -> Double -> Double
mydydx3 x y = do
    let r = 0.5
    r * y

-- | Fourth order runge-kutta algorithm calculations
rungekutta4' :: Double -> Double -> Double -> (Double -> Double -> Double) -> Double
rungekutta4' x y h f = do
    let k1 = h * f x y
    let k2 = h * f (x + 0.5 * h) (y + k1 * 0.5)
    let k3 = h * f (x + 0.5 * h) (y + k2 * 0.5)
    let k4 = h * f (x + h) (y + k3)
    y + 1/6.0 * (k1 + 2 * k2 + 2 * k3 + k4)

-- | Fourth order runge-kutta solver
rungekutta4 :: Double -> Double -> Double -> Double -> (Double -> Double -> Double) -> [Double] -> [Double] -> ([Double],[Double])
rungekutta4 x y h n f xx yy = do
    --let y' = trace ("[trace] x= " ++ show x ++ "  y= " ++ show y) rungekutta4' x y h f
    let y' = rungekutta4' x y h f
    if (x >= n)
        then (xx,yy)
        else rungekutta4 (x+h) y' h n f (xx ++ [x+h]) (yy ++ [y'])

-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Welcome to fourth order Runge-Kutta ODE solver!"
    let v = rungekutta4 0 1 0.01 1.0 mydydx1 [] []
    putStrLn . show $ snd v !! ((length $ snd v) - 1)
    let v = rungekutta4 0 0 0.01 1.0 mydydx2 [] []
    putStrLn . show $ snd v !! ((length $ snd v) - 1)
    let v = rungekutta4 0 2 0.01 3 mydydx3 [] []
    putStrLn . show $ snd v !! ((length $ snd v) - 1)
