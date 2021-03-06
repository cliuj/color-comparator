{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Colors
    ( Color (..)
    , RGB (..)
    , rgbToList
    , addHexHash
    , removeHexHash
    , hexToRgbList
    , hexToColor
    , hexToRGB
    , rgbListToRGB
    , validateFromHexColor
    ) where
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isHexDigit, digitToInt, toLower)
import Text.Regex.TDFA

errInvalidHexColorString :: String
errInvalidHexColorString = "Invalid color hex string passed"

hexColorRegex :: String
hexColorRegex = "^[#]?[a-fA-F0-9]{6}$"

addHexHash :: String -> String
addHexHash h
    | head h /= '#' = "#" ++ h
    | otherwise = h

removeHexHash :: String -> String
removeHexHash h
    | head h == '#' = tail h
    | otherwise = h

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
                 } deriving (Generic, Show)
instance FromJSON Color
instance ToJSON Color

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
hexToColor h = Color Nothing hexString' rgb' Nothing
    where
        rgb' = hexToRGB h
        hexString' = removeHexHash h

hexToRGB :: String -> RGB
hexToRGB = rgbListToRGB . hexToRgbList . removeHexHash . validateFromHexColor

validateFromHexColor :: String -> String
validateFromHexColor s
    | isHexColor = s
    | otherwise = error $ errInvalidHexColorString ++ " String: " ++  s
    where
        isHexColor = s =~ hexColorRegex :: Bool
