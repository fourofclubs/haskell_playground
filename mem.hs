module Mem
(Deck,
Value(..),
Suit(..),
Card(..),
allValues,
allSuits,
(/\),
cutAt,
cutHalf,
inF,
outF,
inFTimes,
outFTimes,
toCard,
micholsonStack
) where

import Data.List as List
import Data.Maybe as Maybe

data Value = TWO | THREE | FOUR | FIVE | SIX | SEVEN | EIGHT | NINE | TEN | JACK | QUEEN | KING | ACE deriving (Show, Read, Eq, Ord, Enum, Bounded)
allValues = [minBound .. maxBound] :: [Value]
data Suit = SPADES | HEARTS | CLUBS | DIAMONDS deriving (Show, Read, Eq, Enum, Bounded)
allSuits = [minBound .. maxBound] :: [Suit]
data Card = Card {value :: Value, suit :: Suit} deriving (Eq)
instance Show Card where
    show (Card v s) = show v ++ " of " ++ show s
(/\) = Card

type Deck = [Card]

weave :: [a] -> [a] -> [a]
weave [] as = as
weave as [] = as
weave as bs = (head as) : (weave bs (tail as))

cutAt :: Int -> [a] -> [a]
cutAt n as = (drop n as) ++ (take n as)

cutHalf :: (RealFrac r) => (r -> Int) -> [a] -> ([a],[a])
cutHalf round as = splitAt (round . (/2) . fromIntegral . length $ as) as

outF :: [a] -> [a]
outF as = 
    let (top, bottom) = cutHalf ceiling as
    in weave top bottom

inF :: [a] -> [a]
inF as = 
    let (top, bottom) = cutHalf floor as 
    in weave bottom top

times :: Int -> (a -> a) -> (a -> a)
times n f = foldl (.) id (replicate n f)

outFTimes :: Int -> [a] -> [a]
outFTimes n = times n outF

inFTimes :: Int -> [a] -> [a]
inFTimes n = times n inF

cardNum :: Deck -> Card -> Int
cardNum d = fromJust . fmap (+1) . flip elemIndex d

toCard :: Deck -> Int -> Card
toCard d n = d !! (n-1)

toCard' d n = toCard d ((n-1) `mod` (length d) + 1)

micholsonStack :: Deck
micholsonStack = 
    [KING /\ SPADES, FOUR /\ HEARTS, KING /\ HEARTS, EIGHT /\ CLUBS, NINE /\ DIAMONDS, SIX /\ SPADES, EIGHT /\ DIAMONDS, THREE /\ SPADES, 
    TEN /\ HEARTS, FIVE /\ CLUBS, KING /\ CLUBS, QUEEN /\ SPADES, FIVE /\ HEARTS, NINE /\ SPADES, SEVEN /\ HEARTS, TWO /\ CLUBS, 
    TEN /\ CLUBS, FIVE /\ DIAMONDS, TWO /\ SPADES, FOUR /\ DIAMONDS, TWO /\ HEARTS, QUEEN /\ HEARTS, SEVEN /\ CLUBS, JACK /\ DIAMONDS,
    EIGHT /\ SPADES, TEN /\ DIAMONDS, FIVE /\ SPADES, NINE /\ HEARTS, FOUR /\ CLUBS, QUEEN /\ CLUBS, ACE /\ DIAMONDS, THREE /\ HEARTS,
    JACK /\ SPADES, SIX /\ HEARTS, ACE /\ CLUBS, NINE /\ CLUBS, SEVEN /\ DIAMONDS, FOUR /\ SPADES, SIX /\ DIAMONDS, ACE /\ SPADES,
    JACK /\ HEARTS, SIX /\ CLUBS, KING /\ DIAMONDS, TEN /\ SPADES, QUEEN /\ DIAMONDS, SEVEN /\ SPADES, EIGHT /\ HEARTS, THREE /\ CLUBS,
    JACK /\ CLUBS, THREE /\ DIAMONDS, ACE /\ HEARTS, TWO /\ DIAMONDS]

micholsonNum = cardNum micholsonStack
micholsonCard = toCard micholsonStack