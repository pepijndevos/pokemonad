module Game where

import Card
import Pokemon
import Attacks

import Data.Random.List
import Data.Random.RVar
import Data.Random.Source.DevRandom

import Control.Monad

data Player = Player { pokemon :: [Stack]
		     , hand :: [Card]
		     , deck :: [Card]
		     , discardPile :: [Card]
		     , prizes :: [Card]}

data Field = Field Player Player

instance Show Field where
	show (Field (Player {pokemon=[], hand=hand}) (Player {pokemon=[]})) =
		"\n\n\n\n" ++ show hand
	show (Field (Player {pokemon=[], hand=hand}) (Player {pokemon=active2:pkm2})) =
		show pkm2 ++ "\n" ++
		show active2 ++ "\n\n\n" ++
		show hand
	show (Field (Player {pokemon=active1:pkm1, hand=hand}) (Player {pokemon=[]})) =
		"\n\n" ++
		show active1 ++ "\n" ++
		show pkm1 ++ "\n" ++
		show hand
	show (Field (Player {pokemon=active1:pkm1, hand=hand}) (Player {pokemon=active2:pkm2})) =
		show pkm2 ++ "\n" ++
		show active2 ++ "\n" ++
		show active1 ++ "\n" ++
		show pkm1 ++ "\n" ++
		show hand

activePokemon :: Player -> Maybe Stack
activePokemon (Player {pokemon=[]}) = Nothing
activePokemon (Player {pokemon=pokemon}) = Just $ head pokemon

updatePokemon :: Player -> Stack -> Int -> Player
updatePokemon p @ (Player {pokemon=pokemon}) st idx=
	p { pokemon=replace pokemon st idx}

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
	return $ (read index) - 1
	where 
		formatList l = unlines $ zipWith formatLine [1..] l
		formatLine x y = (show x) ++ " - " ++ (show y)

main = do
	putStrLn "Starting a new game"
	field <- newGame (newDeck, newDeck)
	putStrLn "Generated random decks"
	mainloop field

mainloop field = do
	putStrLn $ show field
	putStrLn "What do you want to do"
	action <- userSelectFromList ["Attack", "Play a card", "Switch Pokemon", "End turn"]
	newField <- mainAction action field
	mainloop newField 

activePlayer :: Field -> Player
activePlayer (Field p1 _) = p1

waitingPlayer :: Field -> Player
waitingPlayer (Field _ p2) = p2

without :: [a] -> Int -> [a]
without l idx =
	let (ys,zs) = splitAt idx l
	in ys ++ (tail zs)

replace :: [a] -> a -> Int -> [a]
replace l r idx =
	let (ys,zs) = splitAt idx l
	in ys ++ r:(tail zs)


mainAction :: Int -> Field -> IO (Field)
mainAction 0 field @ (Field p1 p2) = do
	case ((activePokemon p1), (activePokemon p2)) of
		(Just att@(Stack pkm _ _), Just def) -> do
			index <- userSelectFromList $ attacks pkm
			let (att', def') = performAttack index (att, def)
			mainAction 3 (Field (updatePokemon p1 att' 0) (updatePokemon p2 def' 0))
		(_, _) -> do
			putStrLn "No active Pokemon"
			return field

mainAction 1 field @ (Field active other) = do
	putStrLn "Select a card from your hand"
	let h = hand $ activePlayer field
	index <- userSelectFromList h
	let newHand = without h index
	    newField = (Field (active { hand=newHand }) other)
	playCard newField (h !! index)

mainAction 2 field = do
	putStrLn "Select new active Pokemon"
	return field

mainAction 3 (Field p2 p1) = do
	let card:deck' = deck p1
	    hand' = card:(hand p1)
	return $ Field (p1 {hand=hand', deck=deck'}) p2

playCard :: Field -> Card -> IO (Field)
playCard (Field p1 p2) card@(Pokemon { evolvesFrom=Nothing }) =
	let st = makeStack card
	    pokemon' = st:(pokemon p1) in
	return $ Field (p1 {pokemon=pokemon'}) p2

playCard (Field p1 p2) card = do
	index <- userSelectFromList $ pokemon p1
	let st = (pokemon p1) !! index
	    st' = attach st card
	return (Field (updatePokemon p1 st' index) p2)
