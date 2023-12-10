import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe
import System.IO

filename :: String
filename = "day1/input.txt"

digitStrings =
  [ "1",
    "one",
    "2",
    "two",
    "3",
    "three",
    "4",
    "four",
    "5",
    "five",
    "6",
    "six",
    "7",
    "seven",
    "8",
    "eight",
    "9",
    "nine"
  ]

main :: IO ()
main = do
  contents <- readFile filename
  let lines = splitOn "\n" contents

  let calibrationValues = map getCalibrationValue lines
  let answer = sum calibrationValues
  print answer

firstDigit :: String -> Char
firstDigit = firstDigitInternal digitStrings

firstDigitInternal :: [String] -> String -> Char
firstDigitInternal [] line = firstDigitInternal digitStrings (tail line)
firstDigitInternal digits line = do
  let digit = head digits
  if digit `isPrefixOf` line then getNumericalValue digit else firstDigitInternal (tail digits) line

lastDigit :: String -> Char
lastDigit = lastDigitInternal digitStrings

lastDigitInternal :: [String] -> String -> Char
lastDigitInternal [] line = lastDigitInternal digitStrings (init line)
lastDigitInternal digits line = do
  let digit = head digits
  if digit `isSuffixOf` line then getNumericalValue digit else lastDigitInternal (tail digits) line

getCalibrationValue :: String -> Integer
getCalibrationValue line = do
  read [firstDigit line, lastDigit line] :: Integer

getNumericalValue :: String -> Char
getNumericalValue digitRaw = do
  let digitIndex = fromMaybe 0 (elemIndex digitRaw digitStrings)
  if length digitRaw == 1 then head digitRaw else head (digitStrings !! (digitIndex - 1))
