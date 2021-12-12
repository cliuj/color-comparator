{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import System.Environment
import System.Exit
import System.IO (hPutStrLn, stderr)
import GHC.Generics
import Control.Monad
import Data.List (sortOn, intercalate, nub)
import Data.List.Split
import Data.Map ((!))
import qualified Data.Map as Map

import Data.Maybe (fromMaybe, isJust)

import Options.Applicative

import Colors
import ResultBuilder
import Comparators

term256ColorsJSON :: String
term256ColorsJSON = "term_256_colors.json"

calculateColorResults :: ComparatorFunction -> String -> [Color] -> [Result]
calculateColorResults f fromHex = map getResult
    where
        getResult c = Result c (getDistance' f (rgb' c) rgb'')
        getDistance' f from to = f from to
        rgb' c = rgb c
        rgb'' = hexToRGB fromHex

data RunData = RunData
               { fromHexColorStr :: String
               , colorsData :: [Color]
               , resultsData :: [Result]
               } deriving (Show)

printOutputs :: Maybe RunData -> Int -> IO ()
printOutputs Nothing _ = putStrLn ""
printOutputs (Just runData) n = do
    putStrLn "Results: "
    mapM_ printOutput results
    where
        results = take n (resultsData runData)

filterXtermSystemColors ::  Maybe RunData -> IO (Maybe RunData)
filterXtermSystemColors Nothing = return Nothing
filterXtermSystemColors (Just rd) = return . Just $ rd {resultsData = filtered}
    where
        filtered = filter (not . maybe False isXtermSystemColor . colorId . color) (resultsData rd)
        isXtermSystemColor c = c `elem` [0..15]

runWithFile :: String -> String -> IO (Maybe RunData)
runWithFile _ "" = return Nothing
runWithFile i f = do
    json <- loadColorsFile f
    let
        colors = case json of
            Left err -> error err
            Right c -> c

    -- Compare colors
    let results = sortOn distance colorResults
            where
                colorResults = calculateColorResults weightedEuclideanDistance i colors
    return . pure $ RunData i colors results

runWithStr :: String -> String -> IO (Maybe RunData)
runWithStr _ "" = return Nothing
runWithStr i cs = do
    let
        colors = map hexToColor comparableColors 
            where
                comparableColors = filter getHexColors (splitOneOf ", " cs)
                getHexColors c = not $ null c

    -- Compare colors
    let results = sortOn distance colorResults
            where
                colorResults = calculateColorResults weightedEuclideanDistance i colors
    return . pure $ RunData i colors results

app :: Opts -> IO ()
app opts = do
    let fromHex = removeHexHash $ validateFromHexColor (fromColor opts)

    printOutput $ Result (hexToColor fromHex) 0.0

    let n = nResults opts
    when (isJust $ toColors opts) . join $ printOutputs <$> runWithStr fromHex c <*> return n
    when (isJust $ optFile opts) . join $ printOutputs <$> runWithFile fromHex f <*> return n
    when (useTerm256 opts) . join $ printOutputs <$> (runWithFile fromHex term256ColorsJSON >>= filterXtermSystemColors) <*> return n
        where
            f = fromMaybe "" (optFile opts)
            c = fromMaybe "" (toColors opts)

data Opts = Opts 
            { fromColor :: String
            , toColors :: Maybe String
            , optFile :: Maybe String
            , useTerm256 :: Bool
            , nResults :: Int
            } deriving (Show)
optsParser :: Parser Opts
optsParser = Opts
        <$> strArgument ( metavar "HEX_COLOR" <> help "Hex color string to compare from")
        <*> optional ( strArgument $ metavar "HEX_COLORS" <> help "Hex colors string to compare to")
        <*> optional ( strOption $ long "file" <> short 'f' <> metavar "JSON" <> help "File of colors to compare to")
        <*> switch ( long "term256" <> help "Compare to Term256 colors")
        <*> option auto ( long "nresults" <> short 'n' <> metavar "INT" <> showDefault <> value 10 <> help "Number of returned results (from each type of comparison)")


main :: IO ()
main = app =<< execParser ( info optsParser fullDesc )
