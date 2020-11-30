module DataTypes where

import qualified System.Random

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

data Action = Hit | Stand
  deriving (Show, Eq, Enum)

data Observation = Observation
  { playerScore :: Word,
    playerHasAce :: Bool,
    dealerCardShowing :: Card
  }
  deriving (Show)

data Environment = Environment
  { currentObservation :: Observation,
    dealerHand :: (Card, Card, [Card]), -- Shown card, hidden card, dealt cards
    playerHand :: [Card],
    deck :: [Card],
    randomGenerator :: System.Random.StdGen,
    playerHasStood :: Bool
  }
  deriving (Show)
