module Blackjack where

import Control.Monad
import Control.Monad.Random
import Control.Monad.State
import Data.List (intercalate, sort)
import DataTypes
import qualified System.Random
import System.Random.Shuffle (shuffleM)

baseDeck :: [Card]
baseDeck = concat $ replicate 4 fullSuit
  where
    fullSuit =
      [ Two,
        Three,
        Four,
        Five,
        Six,
        Seven,
        Eight,
        Nine,
        Ten,
        Jack,
        Queen,
        King,
        Ace
      ]

shuffledDeck :: System.Random.StdGen -> ([Card], System.Random.StdGen)
shuffledDeck gen = runRand (shuffleM baseDeck) gen

value :: Card -> Word
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
value Ace = 1

valueStr :: Card -> String
valueStr Two = "2"
valueStr Three = "3"
valueStr Four = "4"
valueStr Five = "5"
valueStr Six = "6"
valueStr Seven = "7"
valueStr Eight = "8"
valueStr Nine = "9"
valueStr Ten = "10"
valueStr Jack = "J"
valueStr Queen = "Q"
valueStr King = "K"
valueStr Ace = "A"

baseScore :: [Card] -> (Word, Bool)
baseScore cards = (score, score <= 11 && Ace `elem` cards)
  where
    score = sum (value <$> cards)

scoreHand :: [Card] -> Word
scoreHand cards = if hasUsableAce then score + 10 else score
  where
    (score, hasUsableAce) = baseScore cards

isNaturalBlackjack :: [Card] -> Bool
isNaturalBlackjack cards = length cards == 2 && sort scores == [1, 10]
  where
    scores = value <$> cards

resetEnv :: (Monad m) => StateT Environment m Observation
resetEnv = do
  bje <- get
  let (newDeck, newGen) = shuffledDeck (randomGenerator bje)
  let ([playerCard1, playerCard2, dealerHiddenCard, dealerShowCard], playDeck) = splitAt 4 newDeck
  let playerHand = [playerCard1, playerCard2]
  let initialObservation = Observation (scoreHand playerHand) (Ace `elem` playerHand) dealerShowCard
  put $
    Environment
      initialObservation
      (dealerShowCard, dealerHiddenCard, [])
      playerHand
      playDeck
      newGen
      False
  return initialObservation

putEnvironment :: (MonadIO m) => StateT Environment m ()
putEnvironment = do
  bje <- get
  let pHand = playerHand bje
  let (dealerShow, dealerHidden, dealerExtraCards) = dealerHand bje
  liftIO $ do
    if playerStop bje
      then do
        let finalDealerHand = dealerShow : dealerHidden : dealerExtraCards
        putStrLn (handToString finalDealerHand)
        print (scoreHand finalDealerHand)
      else do
        putStr (handToString [dealerShow])
        putStrLn " X"
    putStrLn ""
    putStrLn (handToString pHand)
    print (scoreHand pHand)
  where
    handToString hand = intercalate "" (valueStr <$> hand)

stepEnv :: (Monad m) => Action -> StateT Environment m (Observation, Double, Bool)
stepEnv action = do
  bje <- get
  case action of
    Hit -> do
      let (topCard : remainingDeck) = deck bje
          pHand = playerHand bje
          currentObs = currentObservation bje
          newPlayerHand = topCard : pHand
          newScore = scoreHand newPlayerHand
          newObservation = currentObs {playerScore = newScore, playerHasAce = playerHasAce currentObs || topCard == Ace}
      put $ bje {currentObservation = newObservation, playerHand = newPlayerHand, deck = remainingDeck}
      if newScore > 21
        then return (newObservation, 0.0, True)
        else
          if newScore == 21
            then playOutDealerHand
            else return (newObservation, 0.0, False)
    Stand -> do
      put $ bje {playerStop = True}
      playOutDealerHand

playOutDealerHand :: (Monad m) => StateT Environment m (Observation, Double, Bool)
playOutDealerHand = do
  bje <- get
  let (showCard, hiddenCard, restCards) = dealerHand bje
      currentDealerScore = scoreHand (showCard : hiddenCard : restCards)
  if currentDealerScore < 17
    then do
      let (topCard : remainingDeck) = deck bje
      put $ bje {dealerHand = (showCard, hiddenCard, topCard : restCards), deck = remainingDeck}
      playOutDealerHand
    else do
      let playerScore = scoreHand (playerHand bje)
          currentObs = currentObservation bje
      if playerScore > currentDealerScore || currentDealerScore > 21
        then return (currentObs, 1.0, True)
        else
          if playerScore == currentDealerScore
            then return (currentObs, 0.5, True)
            else return (currentObs, 0.0, True)

gameLoop ::
  (MonadIO m) =>
  StateT Environment m Action ->
  StateT Environment m (Observation, Double)
gameLoop chooseAction = do
  putEnvironment
  newAction <- chooseAction
  (newObs, reward, done) <- stepEnv newAction
  if done
    then do
      liftIO $ print reward
      liftIO $ putStrLn "Done"
      putEnvironment
      return (newObs, reward)
    else gameLoop chooseAction

chooseActionUser :: (MonadIO m) => m Action
chooseActionUser = toEnum . read <$> liftIO getLine

startEnvironment :: IO Environment
startEnvironment = do
  gen <- System.Random.getStdGen
  let (d, newGen) = shuffledDeck gen
  return $
    Environment
      (Observation 0 False Ace)
      (Ace, Ace, [])
      []
      []
      gen
      False

blackjack :: IO ()
blackjack = do
  env <- startEnvironment
  env' <- execStateT resetEnv env
  void $ execStateT (gameLoop chooseActionUser) env'

rules :: IO ()
rules = do
  putStrLn "(1) to Hit, (2) to Stay"

otherGame :: IO ()
otherGame = do
  putStrLn "Future improvements, maybe another card game, or an AI simulator?"

validateInput :: String -> Bool
validateInput input
  | input == "1" = True
  | input == "2" = True
  | otherwise = False

getInput :: String -> (String -> Bool) -> IO String
getInput string function = do
  putStrLn string
  answer <- getLine
  if function answer
    then return answer
    else error "Invalid input"
