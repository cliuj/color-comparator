{-# LANGUAGE OverloadedStrings #-}
module ResultBuilder
    ( Result (..)
    , resultToString
    , displayTerm256Color
    , displayRgbColor
    , buildResult
    ) where

import Text.Printf

term256EscStr :: String
term256EscStr = "\ESC[38;5;%dm███████\ESC[0m "

termRgbEscStr :: String
termRgbEscStr = "\ESC[38;2;%d;%d;%dm███████\ESC[0m "

-- Result is the base result output returned. Additional outputs can be appended/prepended
-- to the Result output string.
data Result = Result
              { resultHex :: String
              , resultRgb :: [Int]
              , resultDistance :: Float
              } deriving (Show)

resultToString :: Result -> String
resultToString result = printf "%s %s %s" hex rgb distance
    where
        hex = resultHex result
        rgb = show $ resultRgb result
        distance = show $ resultDistance result

displayTerm256Color :: Int -> String
displayTerm256Color = printf term256EscStr

displayRgbColor :: [Int] -> String
displayRgbColor rgb = printf termRgbEscStr r g b
    where r = head rgb
          g = rgb !! 1
          b = rgb !! 2

buildResult :: String -> [String]-> String
buildResult r s = unwords s ++ " " ++ r ++ "\n"
