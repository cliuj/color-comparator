{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Colors
    ( Color (..)
    , RGB (..)
    , IdMap
    , loadColorsFile
    , rgbToList
    , createIdMap
    , normalizeColorHex
    , hexToRgbList
    , hexToColor
    , rgbListToRGB
    , validateInputHexColor
    ) where
import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as B

import Text.Regex.TDFA

import Data.Maybe (isJust, fromJust)

import Data.Char (isHexDigit, digitToInt, toLower)


errInvalidInput = "Invalid color hex string passed"

hexColorRegex :: String
hexColorRegex = "^[#]?[a-fA-F0-9]{6}$"

normalizeColorHex :: String -> String
normalizeColorHex s
    | head s == '#' = tail s
    | otherwise = s


loadColorsFile :: String -> IO (Either String [Color])
loadColorsFile f = eitherDecode <$> B.readFile f

data RGB = RGB
           { r :: Int
           , g :: Int
           , b :: Int
           } deriving (Generic, Show)
instance FromJSON RGB
instance ToJSON RGB

rgbToList :: RGB -> [Int]
rgbToList rgb = [r rgb, g rgb, b rgb]

data HSL = HSL
           { h :: Float
           , s :: Float
           , l :: Float
           } deriving (Generic, Show)
instance FromJSON HSL
instance ToJSON HSL

data Color = Color
                 { colorId :: Maybe Int
                 , hexString ::String 
                 , rgb :: RGB
                 , hsl :: Maybe HSL
                 , name :: Maybe String
                 } deriving (Generic, Show)
instance FromJSON Color
instance ToJSON Color

type IdMap = Map String Int

createIdMap :: [Color] -> IdMap
createIdMap colors = Map.fromList $ map mapID colorsWithIDs
    where
        mapID color = (hexString color, fromJust $ colorId color)
        colorsWithIDs = filter (isJust . colorId) colors

hexToDecimal :: String -> Int
hexToDecimal "" = 0
hexToDecimal f = charToHex' (head f) * 16 ^ (length f - 1) + hexToDecimal (tail f)
    where
        charToHex' c
            | ch == 'a' = 10
            | ch == 'b' = 11
            | ch == 'c' = 12
            | ch == 'd' = 13
            | ch == 'e' = 14
            | ch == 'f' = 15
            | otherwise = digitToInt c
            where
                ch = toLower c

hexToRgbList :: String -> [Int]
hexToRgbList "" = []
hexToRgbList hex = hexToDecimal (take 2 hex) : hexToRgbList (drop 2 hex)

rgbListToRGB :: [Int] -> RGB
rgbListToRGB rgbs = RGB red green blue
    where
        red = head rgbs
        green = rgbs !! 1
        blue = rgbs !! 2

hexToColor :: String -> Color
hexToColor h = Color Nothing hexString' rgb' Nothing Nothing
    where
        rgb' = rgbListToRGB $ hexToRgbList $ normalizeColorHex $ validateInputHexColor h
        hexString' = if head h /= '#' then "#" ++ h else h

validateInputHexColor :: String -> String
validateInputHexColor s
    | isHexColor = s
    | otherwise = error errInvalidInput
    where
        isHexColor = s =~ hexColorRegex :: Bool
