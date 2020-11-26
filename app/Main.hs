module Main where

-- Possible cards datastructure
data Card
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Show, Eq, Enum)

-- Represents the score corresponding to each Card
cardPoint :: Card -> Word
cardPoint Ace = 1
cardPoint Two = 2
cardPoint Three = 3
cardPoint Four = 4
cardPoint Five = 5
cardPoint Six = 6
cardPoint Seven = 7
cardPoint Eight = 8
cardPoint Nine = 9
cardPoint Ten = 10
cardPoint Jack = 10
cardPoint Queen = 10
cardPoint King = 10

-- Returns sum of list of cards, accounting for Ace possibility of being 11
baseScore :: [Card] -> Word
baseScore cards = if score <= 11 && Ace `elem` cards then score + 10 else score
  where
    score = sum (cardPoint <$> cards)

main :: IO ()
main = putStrLn "Hello from Haskell!"
