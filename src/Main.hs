{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import System.Environment
import GHC.Generics
import Control.Monad
import Data.List (sortOn, intercalate, nub)
import Data.Char (isHexDigit, digitToInt, toLower)
import Text.Printf

import Term256Colors
import Comparators

-- Given an input color property (RGB or hex string), find
-- the closest Term 256 color using the Euclidean distance
-- formula.

errInvalidInput = "Invalid color hex string passed"
errTooManyInputsGiven = "Only one input is supported"
errNoInputsGiven = "No inputs given"

term256EscStr :: String
term256EscStr = "\ESC[38;5;%dm███████\ESC[0m "

termRgbEscStr :: String
termRgbEscStr = "\ESC[38;2;%d;%d;%dm███████\ESC[0m "


data Result = Result
              { resultColorId :: Maybe Int
              , resultHex :: String
              , resultRgb :: [Int]
              , resultDistance :: Float
              } deriving (Generic, Show)
resultToString :: Result -> String
resultToString result = printf "%v%s %s %s" display hex rgb distance
    where
        hex = resultHex result
        rgb = show $ resultRgb result
        distance = show $ resultDistance result
        display = case resultColorId result of
            Just id -> displayTerm256Color id
            Nothing -> ""

displayTerm256Color :: Int -> String
displayTerm256Color id = printf term256EscStr id

displayRgbColor :: [Int] -> String
displayRgbColor rgb = printf termRgbEscStr r g b
    where r = rgb !! 0
          g = rgb !! 1
          b = rgb !! 2

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

getClosestTerm256Colors :: DistanceFunction -> String -> [Term256Color] -> [Result]
getClosestTerm256Colors f inputHex = map result
    where result tc = Result (Just $ colorId tc) (hexString tc) (rgb' tc) (getDistance' f (rgb' tc) (hexToRgb inputHex))
          getDistance' f from to = f from to
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
    let results = sortOn resultDistance closestColors
            where closestColors = getClosestTerm256Colors weightedEuclideanDistance inputHex filteredTerm256Colors

    -- Output results
    putStrLn "Input: " 
    printf "%v#%s %v %v\n\n" (displayRgbColor $ hexToRgb inputHex) (inputHex) (show $ hexToRgb inputHex) (0.0 :: Float)
    putStrLn "Results: "
    mapM_ (putStrLn . resultToString) $ take 15 results
