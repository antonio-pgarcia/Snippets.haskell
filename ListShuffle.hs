-- | Main entry point to the application.
module Main where

import System.Random
import Debug.Trace

-- | Pop one element from a specific list position
pop :: Int -> [a] -> ([a],[a])
pop _ [] = ([],[])
pop i l = do
    let f = fst $ splitAt (i-1) l
    let s = snd $ splitAt (i-1) l
    let e = [head $ s]
    let ll = f ++ drop 1 s
    (e,ll)

-- | Generate random number within interval a-b
myrandom :: Int -> Int -> Int -> Int
myrandom a b s = do
    fst $ randomR (a,b) (mkStdGen s)

-- | Shuffle a list
shuffle :: [a] -> [a]
shuffle [] = []
shuffle l = do
    shuffle' (myrandom 0 ((length l)-1) 0) l []

-- | Shuffle' auxiliary
shuffle' :: Int -> [a] -> [a] -> [a]
shuffle' r l1 l2 = do
    let len = length l1
    -- let t = trace ("[trace] length l1: " ++ show len) (pop r l1)
    let t = (pop r l1)
    if (len == 0)
        then l2
        else shuffle' (myrandom 0 (len-1) r) (snd $ t) (l2 ++ (fst $ t))

-- | The main entry point.
main :: IO ()
main = do
    let v = [1..100]
    putStrLn "List shuffle!"
    putStrLn . show $ shuffle v
