{-# LANGUAGE OverloadedStrings #-}
module ResultBuilder
    ( Result (..)
    , resultToStr
    , displayRgbColor
    , buildOutput
    , printOutput
    ) where

import Text.Printf
import Data.Maybe (fromMaybe)

import Colors (Color(..), RGB(..), addHexHash)

xtermRgbEscStr :: String
xtermRgbEscStr = "\ESC[38;2;%d;%d;%dm███████\ESC[0m "

-- Result is the base result output returned. Additional outputs can be appended/prepended
-- to the Result output string.
data Result = Result
              { color :: Color
              , distance :: Float
              } deriving (Show)

data ResultAddOns = ResultAddOns
                    { xtermDisplayColor :: String
                    , xtermId :: String
                    }

resultToStr :: Result -> String
resultToStr result = printf " %s  %s  %.2f" hexString' rgb' distance'
    where
        hexString' = addHexHash . hexString $ color result
        rgb' = printf "(%3d, %3d, %3d)" (r rgbs) (g rgbs) (b rgbs) :: String
        rgbs = rgb $ color result
        distance' = distance result

displayRgbColor :: RGB -> String
displayRgbColor rgb = printf xtermRgbEscStr (r rgb) (g rgb) (b rgb)

buildOutput :: Result -> ResultAddOns-> String
buildOutput r a = tc ++ id ++ " " ++ rs ++ "\n"
    where
        rs = resultToStr r
        tc = xtermDisplayColor a
        id = printf "%3s" (xtermId a)

printOutput :: Result -> IO ()
printOutput r = putStr $ buildOutput r (ResultAddOns displayColor id)
    where
        displayColor = displayRgbColor . rgb $ color r
        id = maybe "" show . colorId $ color r
