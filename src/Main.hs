{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import System.Environment
import GHC.Generics
import Control.Monad
import Data.List (sortOn, intercalate, nub)
import Data.Char (isHexDigit, digitToInt, toLower)
import Data.Aeson
import Data.Aeson.TH

import Term256Colors
import Comparators

-- Given an input color property (RGB or hex string), find
-- the closest Term 256 color using the Euclidean distance
-- formula.

errInvalidInput = "Invalid color hex string passed"
errTooManyInputsGiven = "Only one input is supported"
errNoInputsGiven = "No inputs given"

data CmpResult = CmpResult
                 { cmpResultHex :: String
                 , cmpResultRgb :: [Int]
                 , cmpResultDistance :: Float
                 } deriving (Generic, Show)
cmpResultToString :: CmpResult -> String
cmpResultToString result = unwords [cmpResultHex result, show $ cmpResultRgb result, show $ cmpResultDistance result]

normalizeColorHex :: String -> String
normalizeColorHex s
    | head s == '#' = tail s
    | otherwise = s

validateInput :: String -> Either String String
validateInput s
        | length ns /= 6 = Left errInvalidInput
        | not $ isHexString' ns = Left errInvalidInput
        | otherwise = Right ns
    where isHexString' = all isHexDigit
          ns = normalizeColorHex s

hexToDecimal :: String -> Int
hexToDecimal "" = 0
hexToDecimal f = charToHex' (head f) * 16 ^ (length f - 1) + hexToDecimal (tail f)
    where charToHex' c
            | ch == 'a' = 10
            | ch == 'b' = 11
            | ch == 'c' = 12
            | ch == 'd' = 13
            | ch == 'e' = 14
            | ch == 'f' = 15
            | otherwise = digitToInt c
            where ch = toLower c

hexToRgb :: String -> [Int]
hexToRgb "" = []
hexToRgb hex = hexToDecimal (take 2 hex) : hexToRgb (drop 2 hex)

getClosestTerm256Colors :: DistanceFunction -> String -> [Term256Color] -> [CmpResult]
getClosestTerm256Colors f inputHex = map (\tc -> CmpResult (hexString tc) (rgb' tc) (getDistance' f (rgb' tc) (hexToRgb inputHex)))
    where getDistance' f from to = f from to
          rgb' c = rgbToList $ rgb c

validateArgs :: [String] -> String
validateArgs i
    | null i = error errNoInputsGiven
    | length i > 1 = error errTooManyInputsGiven
    | otherwise = inputHex
    where inputHex = case validateInput $ last i of
            Left err -> error err
            Right s -> s
     
main :: IO ()
main = do
    -- Load the contents of the term 256 JSON.
    json <- loadTerm256ColorsFile
    let term256Colors = case json of
            Left err -> error err
            Right colors -> colors

    -- Remove grey, black, and whites from the possible list because they shouldn't
    -- match against a color.
    -- TODO: This should be a passable flag
    let filteredTerm256Colors = filter (\termColor -> length (nub $ rgbToList $ rgb termColor) > 1) term256Colors

    -- Read user input
    i <- getArgs
    let inputHex = validateArgs i

    -- Compare colors
    let results = sortOn cmpResultDistance closestColors
            where closestColors = getClosestTerm256Colors weightedEuclideanDistance inputHex filteredTerm256Colors

    -- Output results
    putStrLn "Input: " 
    putStrLn $ inputHex ++ " " ++  show (hexToRgb inputHex)
    putStrLn ""
    putStrLn "Results: "
    mapM_ (putStrLn . cmpResultToString) $ take 15 results

