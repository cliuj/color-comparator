module Comparators 
    ( euclideanDistance
    , weightedEuclideanDistance
    )where

-- Helpers
square :: Int -> Int
square x = x * x

euclideanDistance :: [Int] -> [Int] -> Float
euclideanDistance a b = sqrt . fromIntegral $ sum $ map square $ zipWith (-) a b

-- Low-cost approximation of Euclidean Distance
-- Taken from https://www.compuphase.com/cmetric.htm
weightedEuclideanDistance :: [Int] -> [Int] -> Float
weightedEuclideanDistance a b = do
    let r1 = fromIntegral $ head a :: Float
    let r2 = fromIntegral $ head b :: Float
    let r = (r1 + r2) / 2
    let dist = map square $ zipWith (-) a b
    let weightG = 4.0
    let weightR = 2 + r / 256
    let weightB = 2 + ((255 - r) / 256)
    sqrt $ weightR * fromIntegral (head dist) + weightG * fromIntegral (dist !! 1) + weightB * fromIntegral (last dist)
