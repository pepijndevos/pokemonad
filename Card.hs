module Card (Card(..)
	    ,Counter(..)
	    ,Colour(..)) where

import Pokemon

data Card = Trainer Effect
	  | Energy Colour
	  | Pokemon { name :: Pokemon
		    , colour :: Colour
		    , hitPoints :: Int
		    , evolvesFrom :: Pokemon
		    , attacks :: [Effect]
		    , retreatCost :: Int
		    , weakness :: Maybe Colour
		    , resistance :: Maybe Colour}
	  deriving(Eq, Show)

data Stack = Stack Card [Card] [Counter]
	   deriving(Eq, Show)

type Effect = Card -> Card

data Counter = DamageCounter | PoisonCounter | SleepCounter | ParalyzeCounter | ConfusionCounter
	     deriving(Eq, Show)

data Colour = Grass | Fire | Water | Lightning | Fighting | Psychic | Colorless
	    deriving(Eq, Show)
