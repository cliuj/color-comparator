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

import Text.Regex.TDFA

import Options.Applicative

import Colors
import ResultBuilder
import Comparators

-- Given an input color property (RGB or hex string), find
-- the closest Term 256 color using the Euclidean distance
-- formula.

term256ColorsJSON :: String
term256ColorsJSON = "term_256_colors.json"

errInvalidInput = "Invalid color hex string passed"

hexColorRegex :: String
hexColorRegex = "^[#]?[a-fA-F0-9]{6}$"

normalizeColorHex :: String -> String
normalizeColorHex s
    | head s == '#' = tail s
    | otherwise = s

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

hexToRgbList :: String -> [Int]
hexToRgbList "" = []
hexToRgbList hex = hexToDecimal (take 2 hex) : hexToRgbList (drop 2 hex)

rgbListToRGB :: [Int] -> RGB
rgbListToRGB rgbs = RGB red green blue
    where red = head rgbs
          green = rgbs !! 1
          blue = rgbs !! 2

calculateColorResults :: ComparatorFunction -> String -> [Color] -> [Result]
calculateColorResults f inputHex = map getResult
    where getResult c = Result (hexString' c) (rgb' c) (getDistance' f (rgb' c) (rgbListToRGB $ hexToRgbList inputHex))
          getDistance' f from to = f from to
          rgb' = rgb :: Color -> RGB
          hexString' = hexString :: Color -> String

validateArgs :: String -> String
validateArgs s
    | isHexColor = normalizeColorHex s
    | otherwise = error errInvalidInput
    where isHexColor = s =~ hexColorRegex :: Bool

app :: Opts -> IO ()
app (Opts i (Just f)) = do
    json <- loadJSONFile f
    let colors = case json of
            Left err -> error err
            Right c -> c
    
    let inputHex = validateArgs i
    let inputResultString = resultToString $ Result inputHex' rgb' dist'
            where rgb' = rgbListToRGB $ hexToRgbList inputHex
                  dist' = 0.0 :: Float
                  inputHex' = "#" ++ inputHex

    -- Compare colors
    let results = sortOn distance colorResults
            where colorResults = calculateColorResults weightedEuclideanDistance inputHex colors

    let topResults = take 15 results

    -- Output results
    putStrLn "Input: " 
    putStrLn $ buildResult inputResultString [ displayRgbColor $ rgbListToRGB $ hexToRgbList inputHex ]
    putStrLn "Results: "
    mapM_ (`printResult` Nothing) topResults

    
app (Opts i Nothing) = do
    -- Load the contents of the term 256 JSON.
    json <- loadJSONFile term256ColorsJSON
    let colors = case json of
            Left err -> error err
            Right c -> c
    let idMap = createIdMap colors

    let inputHex = validateArgs i
    let inputResultString = resultToString $ Result inputHex' rgb' dist'
            where rgb' = rgbListToRGB $ hexToRgbList inputHex
                  dist' = 0.0 :: Float
                  inputHex' = "#" ++ inputHex

    -- Compare colors
    let results = sortOn distance colorResults
            where colorResults = calculateColorResults weightedEuclideanDistance inputHex colors

    let topResults = take 15 results

    -- Output results
    putStrLn "Input: " 
    putStrLn $ buildResult inputResultString [ displayRgbColor $ rgbListToRGB $ hexToRgbList inputHex ]
    putStrLn "Results: "
    mapM_ (\r -> printResult r (Just idMap)) topResults

data Opts = Opts 
            { inputColor :: String
            , optFile :: Maybe String
            } deriving (Show)
optsParser :: Parser Opts
optsParser = Opts
        <$> strArgument ( metavar "HEX_COLOR" <> help "Input hex color string")
        <*> optional ( strOption $ long "file" <> short 'f' <> metavar "COLOR FILE" <> help "File of colors to compare to")


main :: IO ()
main = app =<< execParser ( info optsParser fullDesc )
