{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Colors
    ( Color (..)
    , RGB (..)
    , IdMap
    , loadJSONFile
    , rgbToList
    , createIdMap
    ) where
import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as B

loadColorsFile :: String -> IO (Either String [Color])
loadColorsFile fpath = eitherDecode <$> B.readFile fpath :: IO (Either String [Color])

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

loadJSONFile :: String -> IO (Either String [Color])
loadJSONFile f = eitherDecode <$> B.readFile f :: IO (Either String [Color])

type IdMap = Map String (Maybe Int)

createIdMap :: [Color] -> IdMap
createIdMap tcs = Map.fromList (map mapID tcs)
    where mapID tc = (hexString' tc, colorId tc)
            where hexString' = hexString
