-- | Main entry point to the application.
module Main where

-- | Stirling approximation to factorial of n!
factorial :: Double -> Double
factorial n = do
    if (n == 0)
        then 1
        else sqrt (2.0 * pi * n) * (n/exp(1.0)) ** n

-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Welcome to Stirling Factorial Calculator!"
    putStrLn . show $ factorial 10
