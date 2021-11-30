module Comparators 
    ( euclideanDistance
    , weightedEuclideanDistance
    , ComparatorFunction
    ) where

import Colors (RGB (..), rgbToList)

-- Helpers
square :: Int -> Int
square x = x * x

-- Comparators
-- NOTE: [Int] -> [Int] -> Float represents [r,g,b] -> [r,g,b] -> distance
type ComparatorFunction = RGB -> RGB -> Float

euclideanDistance :: ComparatorFunction
euclideanDistance a b = sqrt . fromIntegral $ sum $ map square $ zipWith (-) a' b'
    where a' = rgbToList a
          b' = rgbToList b

-- Low-cost approximation of Euclidean Distance
-- Taken from https://www.compuphase.com/cmetric.htm
weightedEuclideanDistance :: ComparatorFunction
weightedEuclideanDistance a b = do
    let rmean = (r1 + r2) / 2
            where r1 = fromIntegral $ r a
                  r2 = fromIntegral $ r b
    let weightG = 4.0
    let weightR = 2 + rmean / 256
    let weightB = 2 + ((255 - rmean) / 256)
    sqrt $ weightR * fromIntegral (head dist) + weightG * fromIntegral (dist !! 1) + weightB * fromIntegral (last dist)
            where dist = map square $ zipWith (-) a' b'
                    where a' = rgbToList a
                          b' = rgbToList b

