module Game where

import Card
import Pokemon

import Data.Random.List
import Data.Random.RVar
import Data.Random.Source.DevRandom

import Control.Monad

data Player = Player { pokemon :: [Stack]
		     , hand :: [Card]
		     , deck :: [Card]
		     , discardPile :: [Card]
		     , prizes :: [Card]}
	deriving(Show)

data Field = Field Player Player
	deriving(Show)

activePokemon :: Player -> Maybe Stack
activePokemon (Player {pokemon=[]}) = Nothing
activePokemon (Player {pokemon=pokemon}) = Just $ head pokemon

newPlayer :: [Card] -> IO (Player)
newPlayer deck = do
	shuffled <- runRVar (shuffle deck) DevRandom
	let (hand, restOfTheDeck) = splitAt 7 shuffled
	let (prizes, restOfRestOfTheDeck) = splitAt 6 restOfTheDeck
	return (Player [] hand restOfRestOfTheDeck [] prizes)
	
newGame :: ([Card], [Card]) -> IO (Field)
newGame (deck1, deck2) = do
	player1 <- newPlayer deck1
	player2 <- newPlayer deck2
	return $ Field player1 player2

newDeck :: [Card]
newDeck = take 60 (cycle [pikachu, oddish, gloom, (Energy Lightning), (Energy Grass)])

userSelectFromList :: (Show a) => [a] -> IO (Int)
userSelectFromList l = do
	putStrLn $ formatList l
	index <- getLine
	return $ read index
	where 
		formatList l = unlines $ zipWith formatLine [1..] l
		formatLine x y = (show x) ++ " - " ++ (show y)

main = do
	field <- newGame (newDeck, newDeck)
	mainloop field

mainloop field = do
	putStrLn "What do you want to do"
	action <- userSelectFromList ["Attack", "Play a card", "Switch Pokemon"]
	newField <- mainAction action field
	mainloop newField 

activePlayer :: Field -> Player
activePlayer (Field p1 _) = p1

without :: [a] -> Int -> [a]
without l idx =
	let (ys,zs) = splitAt idx l
	in ys ++ (tail zs)

mainAction :: Int -> Field -> IO (Field)
mainAction 1 field = do
	case (activePokemon (activePlayer field)) of
		Just (Stack pkm _ _) -> putStrLn $ show $ attacks pkm
		Nothing -> putStrLn "No active Pokemon"
	return field

mainAction 2 field @ (Field active other) = do
	putStrLn "Select a card from your hand"
	let h = hand $ activePlayer field
	index <- userSelectFromList h
	let newHand = without h (index - 1)
	    newField = (Field (active { hand=newHand }) other)
	return $ playCard newField (h !! (index - 1))

mainAction 3 field = do
	putStrLn "Select new active Pokemon"
	return field

playCard :: Field -> Card -> Field
playCard field card = field
