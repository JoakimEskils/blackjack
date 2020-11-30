module Main where

import Blackjack

main :: IO ()
main = do
  input <- getInput "Do you want to play blackjack (1) or ???? (2)?" validateInput
  if input == "1"
    then blackjack
    else otherGame