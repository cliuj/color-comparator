{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Term256Colors
    ( RGB (..)
    , HSL (..)
    , Term256Color (..)
    , loadTerm256ColorsFile
    , rgbToList
    ) where

import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as B

term256ColorsJSON :: String
term256ColorsJSON = "term_256_colors.json"

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

data Term256Color = Term256Color
                 { colorId :: Int
                 , hexString ::String 
                 , rgb :: RGB
                 , hsl :: HSL
                 , name :: String
                 } deriving (Generic, Show)
instance FromJSON Term256Color
instance ToJSON Term256Color

loadTerm256ColorsFile :: IO (Either String [Term256Color])
loadTerm256ColorsFile = eitherDecode <$> B.readFile term256ColorsJSON :: IO (Either String [Term256Color])
