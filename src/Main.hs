{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import System.Environment
import GHC.Generics
import Control.Monad
import Data.List (sortOn, intercalate, nub)
import Data.Char (isHexDigit, digitToInt, toLower)

import Data.Map ((!))
import qualified Data.Map as Map

import Colors as C
import ResultBuilder
import Comparators

-- Given an input color property (RGB or hex string), find
-- the closest Term 256 color using the Euclidean distance
-- formula.

errInvalidInput = "Invalid color hex string passed"
errTooManyInputsGiven = "Only one input is supported"
errNoInputsGiven = "No inputs given"

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

calculateColorResults :: DistanceFunction -> String -> [Color] -> [Result]
calculateColorResults f inputHex = map getResult
    where getResult c = Result (hexString' c) (rgb' c) (getDistance' f (rgb' c) (hexToRgb inputHex))
          getDistance' f from to = f from to
          rgb' = rgb :: Color -> [Int]
          hexString' = hexString :: Color -> String

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
            Right c -> c
    let idMap = createIdMap term256Colors
    let colors = convertTerm256Colors term256Colors

    -- Remove grey, black, and whites from the possible list because they shouldn't
    -- match against a color.
    -- TODO: This should be a passable flag
    let filteredColors = filter sameRgb colors
            where sameRgb c = length (nub $ rgb' c) > 1
                    where rgb' = rgb :: Color -> [Int]

    -- Read user input
    i <- getArgs
    let inputHex = validateArgs i
    let inputResultString = resultToString $ Result inputHex' rgb' dist'
            where rgb' = hexToRgb inputHex
                  dist' = 0.0 :: Float
                  inputHex' = "#" ++ inputHex

    -- Compare colors
    let results = sortOn resultDistance colorResults
            where colorResults = calculateColorResults weightedEuclideanDistance inputHex filteredColors

    let topResults = take 15 results

    -- Output results
    putStrLn "Input: " 
    putStrLn $ buildResult inputResultString [ displayRgbColor $ hexToRgb inputHex ]
    putStrLn "Results: "
    mapM_ (\r -> putStr $ buildResult (resultToString r)
                                      [ displayTerm256Color (idMap ! resultHex r)
                                      , show $ idMap ! resultHex r
                                      ]
                                      ) topResults
