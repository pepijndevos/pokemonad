module Pokemon where

import Card
import Attacks

evolve :: Stack -> Card -> Stack
evolve basic@(Stack basicPokemon cards counters) evolution
	| evolvesFrom evolution == Just (name basicPokemon) =
		Stack evolution (basicPokemon:cards) counters
	| otherwise = basic

attachEnergy :: Stack -> Card -> Stack
attachEnergy st@(Stack pkm cards counters) en@(Energy _) =
	Stack pkm (en:cards) counters
attachEnergy st _ = st

pikachu = Pokemon {
	  name = "Pikachu"
	, colour = Lightning
	, hitPoints = 40
	, evolvesFrom = Nothing
	, attacks = [([Energy Colourless], damage 10)]
	, retreatCost = 1
	, weakness = Just Fighting
	, resistance = Nothing}

gloom = Pokemon {
	  name = "Gloom"
	, colour = Grass
	, hitPoints = 60
	, evolvesFrom = Just "Oddish"
	, attacks = [([Energy Grass], poison)]
	, retreatCost = 1
	, weakness = Just Fire
	, resistance = Nothing}

oddish = Pokemon {
	  name = "Oddish"
	, colour = Grass
	, hitPoints = 50
	, evolvesFrom = Nothing
	, attacks = []
	, retreatCost = 1
	, weakness = Just Fire
	, resistance = Nothing}

diglett = Pokemon "Diglett" Fighting 30 Nothing [] 0 (Just Grass) (Just Lightning)
gyarados = Pokemon "Gyarados" Water 100 (Just "Magikarp") [] 3 (Just Grass) (Just Fighting)
magikarp = Pokemon "Magikarp" Water 30 Nothing [] 1 (Just Lightning) Nothing
