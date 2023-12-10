import Control.Monad
import Data.List.Split
import System.IO

filename :: String
filename = "day1/input.txt"

main :: IO ()
main = do
  contents <- readFile filename
  let lines = splitOn "\n" contents
  let calibrationValues = map getCalibrationValue lines
  let answer = sum calibrationValues
  print answer

digits :: String -> String
digits line = [c | c <- line, c `elem` ['0' .. '9']]

firstDigit :: String -> Char
firstDigit line = head (digits line)

lastDigit :: String -> Char
lastDigit line = last (digits line)

getCalibrationValue :: String -> Integer
getCalibrationValue str = read [firstDigit str, lastDigit str] :: Integer
