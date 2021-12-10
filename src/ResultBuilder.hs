{-# LANGUAGE OverloadedStrings #-}
module ResultBuilder
    ( Result (..)
    , resultToString
    , displayTerm256Color
    , displayRgbColor
    , createInputResultString
    , buildOutput
    , printResult
    ) where

import Text.Printf
import Data.Map (lookup)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

import Colors (RGB(..), rgbToList, IdMap, hexToRgbList, rgbListToRGB)

term256EscStr :: String
term256EscStr = "\ESC[38;5;%dm███████\ESC[0m "

termRgbEscStr :: String
termRgbEscStr = "\ESC[38;2;%d;%d;%dm███████\ESC[0m "

-- Result is the base result output returned. Additional outputs can be appended/prepended
-- to the Result output string.
data Result = Result
              { hexString :: String
              , rgb :: RGB
              , distance :: Float
              } deriving (Show)

resultToString :: Result -> String
resultToString result = printf "%s %s %s" hexString' rgb' distance'
    where
        hexString' = hexString result
        rgb' = show $ rgbToList $ rgb result
        distance' = show $ distance result

displayTerm256Color :: Int -> String
displayTerm256Color = printf term256EscStr

displayRgbColor :: RGB -> String
displayRgbColor rgb = printf termRgbEscStr (r rgb) (g rgb) (b rgb)

createInputResultString :: String -> String
createInputResultString i = resultToString $ Result inputHex' rgb' dist'
    where
        rgb' = rgbListToRGB $ hexToRgbList i
        dist' = 0.0
        inputHex' = "#" ++ i

buildOutput :: String -> [String]-> String
buildOutput r s = unwords s ++ " " ++ r ++ "\n"

printResult :: Result -> IdMap -> IO ()
printResult r idMap = putStr $ buildOutput (resultToString r) (displayColor : [ termID ])
    where
        displayColor = maybe (displayRgbColor $ rgb r) displayTerm256Color id
        termID = maybe " " show id
        id = Map.lookup (hexString r) idMap
