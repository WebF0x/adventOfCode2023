import Control.Monad
import Data.List.Split
import GHC.Data.Bag (headMaybe)
import System.IO

data Game = Game
  { gameId :: Integer,
    rounds :: [Round]
  }
  deriving (Show)

data ColorAndCount = ColorAndCount {color :: Color, count :: Integer} deriving (Show)

newtype Round = Round {cubesByColor :: [ColorAndCount]} deriving (Show)

data Color = Red | Green | Blue deriving (Enum, Show, Eq)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let lines = splitOn "\n" contents
  let games = map toGame lines
  let gamePowers :: [Integer] = map toGamePower games
  let answer = sum gamePowers
  print answer

toGame :: String -> Game
toGame line =
  Game
    { gameId = getGameId line,
      rounds = getGameRounds line
    }

getGameId :: String -> Integer
getGameId line = do
  let gameText = head (splitOn ": " line)
  read (last (splitOn " " gameText)) :: Integer

getGameRounds :: String -> [Round]
getGameRounds line = do
  let roundsText = last (splitOn ": " line)
  map toRound (splitOn "; " roundsText)

toRound :: String -> Round
toRound str = do
  Round {cubesByColor = map toColorAndCount (splitOn ", " str)}

toColorAndCount :: String -> ColorAndCount
toColorAndCount cubes = do
  ColorAndCount
    { color = toColor (last (splitOn " " cubes)),
      count = read (head (splitOn " " cubes))
    }

toColor :: String -> Color
toColor color =
  case color of
    "red" -> Red
    "green" -> Green
    "blue" -> Blue

colors = [Red, Green, Blue]

toGamePower :: Game -> Integer
toGamePower game = product [maxColorDraw (rounds game) c | c <- colors]

maxColorDraw :: [Round] -> Color -> Integer
maxColorDraw rounds c = maximum (map (`colorCount` c) rounds)

colorCount :: Round -> Color -> Integer
colorCount round myColor = do
  let maybeColorAndCounts = filter (\colorAndCount -> color colorAndCount == myColor) (cubesByColor round)
  if null maybeColorAndCounts then 0 else count (head maybeColorAndCounts)