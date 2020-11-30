module Blackjack where

import Control.Monad
import Control.Monad.Random
import Control.Monad.State
import System.Random

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

-- Represents the deck of cards
type Deck = [Card]

-- Represents action of player
data Action = Stand | Hit
  deriving (Show, Eq, Enum)

-- Represents the players known information about the game
data Observation = Observation
  { playerScore :: Word,
    playerHasAce :: Bool,
    dealerCardShowing :: Card
  }
  deriving (Show)

-- Represents the current status
data Environment = Environment
  { currObservation :: Observation,
    player :: Deck,
    deck :: Deck,
    dealer :: (Card, Card, Deck),
    rand :: System.Random.StdGen,
    playerStopped :: Bool
  }
  deriving (Show)

-- Represents the score corresponding to each Card
value :: Card -> Word
value Ace = 1
value Two = 2
value Three = 3
value Four = 4
value Five = 5
value Six = 6
value Seven = 7
value Eight = 8
value Nine = 9
value Ten = 10
value Jack = 10
value Queen = 10
value King = 10

-- Returns sum of list of cards, accounting for Ace possibility of being 11
baseScore :: [Card] -> Word
baseScore cards = if score <= 11 && Ace `elem` cards then score + 10 else score
  where
    score = sum (value <$> cards)

-- player option to hit or stay
playerAction :: (MonadIO m) => m Action
playerAction = toEnum . read <$> liftIO getLine

--playerAction :: (MonadIO m) => m Action
--playerAction = do
--putStrLn "Pick action"
--putStrLn "(1) Hit"
--putStrLn "(2) Stay"
--optionVal <- getLine
--return (read optionVal :: Int)

startGame :: (MonadIO m) => m Action
startGame = playerAction