module Comparators 
    ( euclideanDistance
    , weightedEuclideanDistance
    , DistanceFunction
    )where

-- Helpers
square :: Int -> Int
square x = x * x

-- Comparators
-- NOTE: [Int] -> [Int] -> Float represents [r,g,b] -> [r,g,b] -> distance
type DistanceFunction = [Int] -> [Int] -> Float

euclideanDistance :: DistanceFunction
euclideanDistance a b = sqrt . fromIntegral $ sum $ map square $ zipWith (-) a b

-- Low-cost approximation of Euclidean Distance
-- Taken from https://www.compuphase.com/cmetric.htm
weightedEuclideanDistance :: DistanceFunction
weightedEuclideanDistance a b = do
    let r = (r1 + r2) / 2
            where r1 = fromIntegral $ head a :: Float
                  r2 = fromIntegral $ head a :: Float
    let weightG = 4.0
    let weightR = 2 + r / 256
    let weightB = 2 + ((255 - r) / 256)
    sqrt $ weightR * fromIntegral (head dist) + weightG * fromIntegral (dist !! 1) + weightB * fromIntegral (last dist)
            where dist = map square $ zipWith (-) a b

