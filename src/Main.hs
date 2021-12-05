{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import System.Environment
import GHC.Generics
import Control.Monad
import Data.List (sortOn, intercalate, nub)
import Data.Map ((!))
import qualified Data.Map as Map

import Data.Maybe (fromMaybe)

import Text.Regex.TDFA

import Options.Applicative

import Colors as C
import ResultBuilder as RB
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

calculateColorResults :: ComparatorFunction -> String -> [Color] -> [Result]
calculateColorResults f inputHex = map getResult
    where
        getResult c = Result (hexString' c) (rgb' c) (getDistance' f (rgb' c) rgb'')
        getDistance' f from to = f from to
        rgb' c = C.rgb c
        rgb'' = rgbListToRGB $ hexToRgbList inputHex
        hexString' = C.hexString

validateInputHexColor :: String -> String
validateInputHexColor s
    | isHexColor = s
    | otherwise = error errInvalidInput
    where
        isHexColor = s =~ hexColorRegex :: Bool

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
printOutput (Just runData) = mapM_ (\r -> printResult r (idMap runData)) nResults
    where
        nResults = take 15 (resultsData runData)

run :: String -> String -> IO (Maybe RunData)
run _ "" = return Nothing
run i f = do
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

app :: Opts -> IO ()
app opts = do
    let inputHex = normalizeColorHex $ validateInputHexColor (inputColor opts)
    printInput inputHex
    putStrLn "Results: "
    run inputHex f >>= printOutput
        where
            f = fromMaybe term256ColorsJSON (optFile opts)

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
