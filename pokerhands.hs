module PokerHands
  ( Hand (..),
    HandBuilder (..),
    buildHand,
    initialBuilder,
  )
where

import Data.List as L
import Data.Map as M
import Mem

data HandType = PAIR | TWO_PAIR | TRIPS | STRAIGHT | FLUSH | FULL_HOUSE | QUADS | STRAIGHT_FLUSH deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Hand = Pair Value | TwoPair Value Value | Trips Value | Straight Value | Flush Suit Value | FullHouse Value Value | Quads Value | StraightFlush Value Suit deriving (Eq)

instance Show Hand where
  show (Pair v) = "Pair of " ++ show v ++ "s"
  show (TwoPair v1 v2) = "Two Pair - " ++ show v1 ++ "s and " ++ show v2 ++ "s"
  show (Trips v) = "Three " ++ show v ++ "s"
  show (Straight v) = "Straight to the " ++ show v
  show (Flush s v) = "Flush in " ++ show s ++ " to the " ++ show v
  show (FullHouse v1 v2) = "Full house - " ++ show v1 ++ "s over " ++ show v2 ++ "s"
  show (Quads v) = "Four " ++ show v ++ "s"
  show (StraightFlush v s) = "Straight flush in " ++ show s ++ " to the " ++ show v

data HandBuilder = HandBuilder {prompt :: String, optionList :: [String], selectOption :: Int -> Either HandBuilder Hand}

instance Show HandBuilder where
  show hb = "Building from " ++ show (optionList hb)

handBuilder :: (Show s) => String -> [s] -> (s -> Either HandBuilder Hand) -> HandBuilder
handBuilder p ss f = HandBuilder p (L.map show ss) (f . (ss !!) . subtract 1)

options :: HandBuilder -> [String]
options hb = zipWith (\n s -> show n ++ " : " ++ s) [1 ..] (optionList hb)

pair = handBuilder "Pair of ..?" allValues (Right . Pair)

twoPair' v1 = handBuilder "Second value?" (L.delete v1 allValues) (Right . TwoPair v1)

twoPair = handBuilder "First value?" allValues (Left . twoPair')

trips = handBuilder "Three what?" allValues (Right . Trips)

straight = handBuilder "Straight to the ..?" [FIVE ..] (Right . Straight)

flush' s = handBuilder "High card?" [SEVEN ..] (Right . Flush s)

flush = handBuilder "Flush in ..?" allSuits (Left . flush')

fullHouse' v1 = handBuilder "Over?" (L.delete v1 allValues) (Right . FullHouse v1)

fullHouse = handBuilder "First value?" allValues (Left . fullHouse')

quads = handBuilder "Four what?" allValues (Right . Quads)

straightFlush' v = handBuilder "In ..?" allSuits (Right . StraightFlush v)

straightFlush = handBuilder "Straight flush to the ..?" [FIVE ..] (Left . straightFlush')

handPicker :: HandType -> HandBuilder
handPicker PAIR = pair
handPicker TWO_PAIR = twoPair
handPicker TRIPS = trips
handPicker STRAIGHT = straight
handPicker FLUSH = flush
handPicker FULL_HOUSE = fullHouse
handPicker QUADS = quads
handPicker STRAIGHT_FLUSH = straightFlush

initialBuilder = handBuilder "Choose a hand, any hand..." ([minBound .. maxBound] :: [HandType]) (Left . handPicker)

buildHand :: HandBuilder -> IO Hand
buildHand hb = do
  putStrLn $ prompt hb
  mapM_ putStrLn (options hb)
  selection <- readLn :: IO Int
  either buildHand return (selectOption hb selection)