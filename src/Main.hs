{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import System.Environment
import System.Exit
import System.IO (hPutStrLn, stderr)
import GHC.Generics
import Control.Monad
import Data.List (sortOn)
import Data.List.Split
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import Options.Applicative

import Colors
import ResultBuilder
import Comparators

xterm256ColorsJSON :: String
xterm256ColorsJSON = "xterm256Colors.json"

data ColorSource = ColorsFromFile String | ColorsFromStr String

loadColorsFile :: String -> IO (Either String [Color])
loadColorsFile f = A.eitherDecode <$> B.readFile f

calculateColorResults :: ComparatorFunction -> String -> [Color] -> [Result]
calculateColorResults f fromHex = map getResult
    where
        getResult c = Result c (getDistance' f (rgb' c) rgb'')
        getDistance' f from to = f from to
        rgb' c = rgb c
        rgb'' = hexToRGB fromHex

printOutputs :: String -> Int -> [Result] -> IO ()
printOutputs s _ [] = putStrLn s
printOutputs s n r = do
    putStrLn $ "\nResults compared to: " ++ s
    mapM_ printResult results
    where
        results = take n (sortOn distance r)

filterXtermSystemColors ::  [Result] -> [Result]
filterXtermSystemColors = filter (not . maybe False isXtermSystemColor . colorId . color)
    where 
        isXtermSystemColor c = c `elem` [0..15]

getColors :: ColorSource -> IO [Color]
getColors (ColorsFromFile "") = return []
getColors (ColorsFromStr "") = return []
getColors (ColorsFromFile f) = do
    json <- loadColorsFile f
    case json of
        Left err -> error err
        Right c -> return c
getColors (ColorsFromStr s) = return $ map hexToColor comparableColors
    where
        comparableColors = filter (not . null) (splitOneOf ", " s)

getResults :: Color -> [Color] -> [Result]
getResults fromColor = calculateColorResults weightedEuclideanDistance (hexString fromColor)

runApp :: Opts -> IO ()
runApp opts = do
    let
        fromHex = hexToColor . removeHexHash . validateFromHexColor $ fromColor opts
        n = nResults opts
        f = fromMaybe "" (compareFile opts)
        s = fromMaybe "" (toColors opts)
    
    putStrLn "From:"; printResult $ Result fromHex 0.0
    strResults <- getResults fromHex <$> getColors (ColorsFromStr s)
    fileResults <- getResults fromHex <$> getColors (ColorsFromFile f)
    xtermResults <- getResults fromHex <$> getColors (ColorsFromFile xterm256ColorsJSON)

    if mergeResults opts then
        printOutputs "merged" n . concat $ [ strResults, fileResults, xtermResults ]
    else do
        when (isJust $ toColors opts) (printOutputs s n strResults)
        when (isJust $ compareFile opts) (printOutputs f n fileResults)
        when (compareXterm256 opts) (printOutputs "xterm256" n (filterXtermSystemColors xtermResults))

data Opts = Opts 
            { fromColor :: String
            , toColors :: Maybe String
            , compareFile :: Maybe String
            , compareXterm256 :: Bool
            , nResults :: Int
            , mergeResults :: Bool
            } deriving (Show)
optsParser :: Parser Opts
optsParser = Opts
        <$> strArgument ( metavar "HEX_COLOR" <> help "Hex color string to compare from")
        <*> optional ( strArgument $ metavar "HEX_COLORS" <> help "Hex colors string to compare to")
        <*> optional ( strOption $ long "file" <> short 'f' <> metavar "JSON" <> help "File of colors to compare to")
        <*> switch ( long "xterm256" <> help "Compare to xterm256 colors")
        <*> option auto ( long "nresults" <> short 'n' <> metavar "INT" <> showDefault <> value 10 <> help "Number of returned results (from each type of comparison)")
        <*> switch ( long "merge" <> help "Merge different source comparisons into one single result output")


main :: IO ()
main = runApp =<< execParser ( info optsParser fullDesc )
