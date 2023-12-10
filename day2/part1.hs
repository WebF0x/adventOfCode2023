import Control.Monad
import Data.List.Split
import System.IO

data Game = Game
  { gameId :: Integer,
    rounds :: [Round]
  }
  deriving (Show)

data ColorAndCount = ColorAndCount {color :: Color, count :: Integer} deriving (Show)

newtype Round = Round {cubesByColor :: [ColorAndCount]} deriving (Show)

data Color = Red | Green | Blue deriving (Enum, Show, Eq)

rules =
  [ ColorAndCount {color = Red, count = 12},
    ColorAndCount {color = Green, count = 13},
    ColorAndCount {color = Blue, count = 14}
  ]

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let lines = splitOn "\n" contents
  let games = map toGame lines
  let possibleGames :: [Game] = filter isGamePossible games
  let answer = sumOfIds possibleGames
  print answer

isGamePossible :: Game -> Bool
isGamePossible game = all isRoundPossible (rounds game)

isRoundPossible :: Round -> Bool
isRoundPossible round = all cubeColorAndCountIsPossible (cubesByColor round)

getRule :: Color -> ColorAndCount
getRule cubeColor = head (filter (\rule -> color rule == cubeColor) rules)

cubeColorAndCountIsPossible :: ColorAndCount -> Bool
cubeColorAndCountIsPossible cubes = do
  let rule :: ColorAndCount = getRule (color cubes)
  count cubes <= count rule

sumOfIds :: [Game] -> Integer
sumOfIds games = sum (map gameId games)

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