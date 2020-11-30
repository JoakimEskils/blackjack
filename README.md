# blackjack

## Info

This project uses Stack. For documentation:
https://docs.haskellstack.org/en/stable/README/

Update: Currently there are some error regarding score when using (1) Hit in the game. Further investigation needed.

## Installation & Running

If you don't have stack:

- brew install haskell-stack

Run program from root folder:

- stack build

- stack exec -- runghc app/Main.hs

OR

- stack exec -- ghc Main.hs -o BlackJack
- ./BlackJack 
