module Attacks where

import Card
import Data.List

addCounter :: Counter -> Int -> Stack -> Stack
addCounter cnt num (Stack pkm cards counters) =
	let newCounters = counters ++ take num (repeat cnt)
	in Stack pkm cards newCounters

weaknessResistance :: Colour -> Maybe Colour -> Maybe Colour -> Int -> Int
weaknessResistance colour weakness resistance dmg
	| Just colour == resistance = dmg - 30
	| Just colour == weakness   = dmg * 2
	| otherwise                 = dmg

damage :: Int -> Effect
damage hp (attacker @ (Stack att _ _), defender @ (Stack def _ _)) =
	let realDamage = (weaknessResistance (colour att) (weakness def) (resistance def) hp)
	in (attacker, addCounter DamageCounter (realDamage `div` 10)  defender)

isColourless :: Card -> Bool
isColourless (Energy Colourless) = True
isColourless _ = False

payColoured :: ([Card], [Card]) -> ([Card], [Card])
payColoured (cost, attached) = (remainingCost, remainingEnergy)
	where remainingCost = cost \\ attached
	      remainingEnergy = attached \\ cost 

payRest :: ([Card], [Card]) -> Bool
payRest (cost, attached) = length colourless <= length attached && coloured == []
	where (colourless, coloured) = partition isColourless cost

enoughEnergy :: ([Card], [Card]) -> Bool
enoughEnergy = payRest . payColoured

isEnergy :: Card -> Bool
isEnergy (Energy _) = True
isEnergy _ = False

energy :: Stack -> [Card]
energy (Stack _ cards _) = filter isEnergy  cards

performAttack :: Int -> Opponents -> Opponents
performAttack index (att @ (Stack pkm _ _), def) =
	let (cost, _,  attfn) = (attacks pkm) !! index in
		if enoughEnergy (cost, energy att) then
			attfn (att, def)
		else
			(att, def)

poison :: Effect
poison (att, def @ (Stack _ _ counters))
	| PoisonCounter `elem` counters = (att, def)
	| otherwise 			= (att, addCounter PoisonCounter 1 def)
