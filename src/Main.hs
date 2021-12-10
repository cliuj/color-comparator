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

import Colors as C
import ResultBuilder as RB
import Comparators

-- Given an input color property (RGB or hex string), find
-- the closest Term 256 color using the Euclidean distance
-- formula.

term256ColorsJSON :: String
term256ColorsJSON = "term_256_colors.json"

calculateColorResults :: ComparatorFunction -> String -> [Color] -> [Result]
calculateColorResults f inputHex = map getResult
    where
        getResult c = Result (hexString' c) (rgb' c) (getDistance' f (rgb' c) rgb'')
        getDistance' f from to = f from to
        rgb' c = C.rgb c
        rgb'' = rgbListToRGB $ hexToRgbList inputHex
        hexString' = C.hexString

data RunData = RunData
               { inputHexColorData :: String
               , colorsData :: [Color]
               , resultsData :: [Result]
               , idMap :: IdMap
               } deriving (Show)

printInput :: String -> IO ()
printInput inputHexColor = putStrLn $ "Input:\n" ++ buildOutput resultString [ displayColor ]
    where
        resultString = createInputResultString inputHexColor
        displayColor = displayRgbColor $ rgbListToRGB $ hexToRgbList inputHexColor

printOutput :: Maybe RunData -> IO ()
printOutput Nothing = putStrLn ""
printOutput (Just runData) = do
    putStrLn "Results: "
    mapM_ (\r -> printResult r (idMap runData)) nResults
    where
        nResults = take 15 (resultsData runData)

runWithFile :: String -> String -> IO (Maybe RunData)
runWithFile _ "" = return Nothing
runWithFile i f = do
    json <- loadColorsFile f
    let
        colors = case json of
            Left err -> error err
            Right c -> c
        idMap = createIdMap colors

    -- Compare colors
    let results = sortOn distance colorResults
            where
                colorResults = calculateColorResults weightedEuclideanDistance i colors
    return $ pure $ RunData i colors results idMap

runWithStr :: String -> String -> IO (Maybe RunData)
runWithStr _ "" = return Nothing
runWithStr i cs = do
    let
        colors = map hexToColor comparableColors 
            where
                comparableColors = filter getHexColors (splitOneOf ", " cs)
                getHexColors c = not $ null c
        idMap = createIdMap colors

    -- Compare colors
    let results = sortOn distance colorResults
            where
                colorResults = calculateColorResults weightedEuclideanDistance i colors
    return $ pure $ RunData i colors results idMap

app :: Opts -> IO ()
app opts = do
    let inputHex = C.normalizeColorHex $ C.validateInputHexColor (inputColor opts)
    printInput inputHex

    when (isJust (inputColors opts)) $ runWithStr inputHex c >>= printOutput
    when (isJust (optFile opts)) $ runWithFile inputHex f >>= printOutput
        where
            f = fromMaybe term256ColorsJSON (optFile opts)
            c = fromMaybe "" (inputColors opts)

data Opts = Opts 
            { inputColor :: String
            , inputColors :: Maybe String
            , optFile :: Maybe String
            } deriving (Show)
optsParser :: Parser Opts
optsParser = Opts
        <$> strArgument ( metavar "HEX_COLOR" <> help "Input hex color string")
        <*> optional ( strArgument $ metavar "HEX_COLORS" <> help "Input hex color strings to compare to")
        <*> optional ( strOption $ long "file" <> short 'f' <> metavar "COLOR FILE" <> help "File of colors to compare to")


main :: IO ()
main = app =<< execParser ( info optsParser fullDesc )
