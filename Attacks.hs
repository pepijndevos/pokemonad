module Attacks () where

import Card

damage :: Colour -> Int -> Card -> Card
damage colour dmg poke @ Pokemon { hitPoints=hp, resistance=resistance, weakness=weakness}
	| Just colour == resistance = poke { hitPoints = (hp - (dmg - 30)) }
	| Just colour == weakness   = poke { hitPoints = (hp - (dmg * 2)) }
	| otherwise                 = poke { hitPoints = (hp - dmg) }
