module Card where

import Text.Show.Functions

data Card = Trainer String Effect
	  | Energy Colour
	  | Pokemon { name :: String
		    , colour :: Colour
		    , hitPoints :: Int
		    , evolvesFrom :: Maybe String
		    , attacks :: [([Card], Effect)]
		    , retreatCost :: Int
		    , weakness :: Maybe Colour
		    , resistance :: Maybe Colour}

instance Show Card where
	show (Energy colour) = show colour ++ " energy"
	show (Trainer name _) = "Trainer " ++ name
	show (Pokemon { name=name, hitPoints=hp }) = name ++ " (" ++ show hp ++ "HP)"

instance Eq Card where
	(Energy a) == (Energy b) = (a == b)
	(Trainer a _) == (Trainer b _) = (a == b)
	(Pokemon { name=a }) == (Pokemon { name=b }) = (a == b)

data Stack = Stack Card [Card] [Counter]

instance Show Stack where
	show (Stack pkm cards counters) =
		show pkm ++ " " ++
		concatMap show counters ++ "\n" ++
		unlines (map (\p -> "\t" ++ show p) cards)

type Opponents = (Stack, Stack)
type Effect = Opponents -> Opponents

data Counter = DamageCounter | PoisonCounter | SleepCounter | ParalyzeCounter | ConfusionCounter
	     deriving(Eq)

instance Show Counter where
	show DamageCounter = "(10)"
	show PoisonCounter = "\9760"
	show _ = ""

data Colour = Grass | Fire | Water | Lightning | Fighting | Psychic | Colourless
	    deriving(Eq, Show)

makeStack :: Card -> Stack
makeStack c = Stack c [] []
