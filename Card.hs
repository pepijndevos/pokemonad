module Card (Card(..)
	    ,Counter(..)
	    ,Colour(..)) where

import Pokemon

data Card = Trainer
	  | Energy Colour
	  | Pokemon { name :: Pokemon
		    , hitPoints :: Int
		    , weakness :: Maybe Colour
		    , resistance :: Maybe Colour}
	  deriving(Eq, Show)

data Counter = DamageCounter | PoisonCounter | SleepCounter | ParalyzeCounter | ConfusionCounter
	     deriving(Eq, Show)

data Colour = Grass | Fire | Water | Lightning | Fighting | Psychic | Colorless
	    deriving(Eq, Show)
