{-# LANGUAGE OverloadedStrings #-}
module ResultBuilder
    ( Result (..)
    , resultToString
    , displayTerm256Color
    , displayRgbColor
    , buildResult
    , printResult
    ) where

import Text.Printf
import Data.Map ((!))
import Data.Maybe (fromMaybe)

import Colors (RGB(..), rgbToList, IdMap)

notTermID :: Int
notTermID = -1

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

buildResult :: String -> [String]-> String
buildResult r s = unwords s ++ " " ++ r ++ "\n"

printResult :: Result -> Maybe IdMap -> IO ()
printResult r (Just idMap) = putStr $ buildResult (resultToString r) [ displayColor, termID ]
    where
        displayColor = if id == notTermID then "" else displayTerm256Color id
        termID = if id == notTermID then "" else show id
        id = fromMaybe notTermID (idMap ! hexString r)

printResult r Nothing = putStr $ buildResult (resultToString r) [ displayRgbColor $ (rgb :: Result -> RGB) r ]
