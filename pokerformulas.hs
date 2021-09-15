import Control.Applicative
import Control.Monad
import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Ord
import Mem
import PokerHands

main = do
  hand <- buildHand initialBuilder
  let pd = playerDeals micholsonStack hand
  if M.null pd
    then putStrLn "No deal found."
    else do
      putStrLn "Players?"
      mapM_ print (keys pd)
      players <- readLn :: IO Int
      putStrLn (show hand ++ ", " ++ show players ++ " players:")
      print (pd ! players)

data PredicateType
  = IsValue Value
  | IsSuit Suit
  | IsNotValue Value
  | ValueAtMost Value
  | IsCard Value Suit
  | IsAnything
  | And PredicateType PredicateType
  | Not PredicateType
  deriving (Show, Eq)

toF :: PredicateType -> Card -> Bool
toF (IsValue v) = (== v) . value
toF (IsSuit s) = (== s) . suit
toF (IsNotValue v) = not . toF (IsValue v)
toF (ValueAtMost v) = (<= v) . value
toF (IsCard v s) = (== (v /\ s))
toF (And p1 p2) = \c -> toF p1 c && toF p2 c
toF (Not p') = not . toF p'
toF IsAnything = const True

straightValues :: Value -> [Value]
straightValues v = L.take 5 [v, pred v ..] ++ ([ACE | v <= FIVE])

cardsFor :: Hand -> [PredicateType]
cardsFor (Pair v) = [IsValue v, IsValue v, IsNotValue v, IsNotValue v, IsNotValue v]
cardsFor (TwoPair v1 v2) = [IsValue v1, IsValue v1, IsValue v2, IsValue v2, IsNotValue v1 `And` IsNotValue v2]
cardsFor (Trips v) = [IsValue v, IsValue v, IsValue v, IsNotValue v, IsNotValue v]
cardsFor (Straight v) = L.map IsValue (straightValues v)
cardsFor (Flush s v) = IsCard v s : replicate 4 (And (IsSuit s) (ValueAtMost v))
cardsFor (FullHouse v1 v2) = [IsValue v1, IsValue v1, IsValue v1, IsValue v2, IsValue v2]
cardsFor (Quads v) = [IsValue v, IsValue v, IsValue v, IsValue v, IsAnything]
cardsFor (StraightFlush v s) = L.map (`IsCard` s) (straightValues v)

type Deal = (Int, [Int])

type Players = Int

findLastIndex :: (a -> Bool) -> [a] -> Maybe Int
findLastIndex p as = case findIndices p as of
  [] -> Nothing
  is -> Just (last is)

searchFromTop :: Deck -> Players -> [Card -> Bool] -> Maybe [Int]
searchFromTop d p [] = Just []
searchFromTop d p (cp : cps) = dealOne p cp d >>= (\(n, d') -> fmap (n :) (searchFromTop d' p cps))

dealOne :: Players -> (Card -> Bool) -> Deck -> Maybe (Int, Deck)
dealOne p cp d
  | length d < p = Nothing
  | otherwise = deal p cp d <|> deal' p cp d <|> deal'' p cp d

--second deal to players
deal :: Players -> (Card -> Bool) -> Deck -> Maybe (Int, Deck)
deal p cp d =
  let (top, bottom) = L.splitAt p d
   in fmap (\a -> (a, bottom) . ((`mod` p) . (+ 1))) (findLastIndex cp top)

--second deal to dealer
deal' :: Players -> (Card -> Bool) -> Deck -> Maybe (Int, Deck)
deal' p cp d =
  let (top, bottom) = L.splitAt p d
   in if L.null bottom
        then Nothing
        else if cp (head bottom) then Just (p, last top : tail bottom) else Nothing

--bottom deal
deal'' :: Players -> (Card -> Bool) -> Deck -> Maybe (Int, Deck)
deal'' p cp d =
  let (top, bottom) = L.splitAt p d
   in if L.null bottom
        then Nothing
        else if cp (last bottom) then Just (-1, last top : init bottom) else Nothing

searchFrom :: Int -> Deck -> Players -> [Card -> Bool] -> Maybe [Int]
searchFrom n = searchFromTop . cutAt n

evaluateDeal :: Players -> Deal -> Int
evaluateDeal p d = sum . L.map (\x -> if x == 0 then p + 1 else x) $ snd d

bestDeal :: Players -> [Deal] -> Maybe Deal
bestDeal p [] = Nothing
bestDeal p ds = Just . (maximumBy . comparing $ evaluateDeal p) $ ds

search' :: Deck -> Players -> [Card -> Bool] -> Maybe Deal
search' d p cps =
  let deals = catMaybes [sequence (i, searchFrom i d p cps) | i <- [1 .. length d]]
   in bestDeal p deals

search :: Deck -> Players -> Hand -> Maybe Deal
search d p h =
  let cards = (L.map . L.map $ toF) (nub . permutations . cardsFor $ h)
      deals = mapMaybe (search' d p) cards
   in bestDeal p deals

playerDeals :: Deck -> Hand -> Map Players Deal
playerDeals d h = M.fromList . catMaybes $ [sequence (p, search d p h) | p <- [3 .. 10]]

selectDeal :: Deck -> [Int] -> HandBuilder -> (Hand, Maybe Deal)
selectDeal d (s1 : s2 : ss) hb = either (selectDeal d (s2 : ss)) (\h -> (h, search d s2 h)) (selectOption hb s1)
selectDeal _ _ _ = error "Not enough selections."

data Menu v = Menu String [Menu v] | MenuItem String v deriving (Show, Eq)

instance Functor Menu where
  fmap f (MenuItem s v) = MenuItem s (f v)
  fmap f (Menu s ms) = Menu s (L.map (fmap f) ms)

menuItem :: Show v => v -> Menu v
menuItem v = MenuItem (show v) v

menuItems :: Show k => [(k, v)] -> [Menu v]
menuItems = L.map (\(k, v) -> MenuItem (show k) v)

initialMenu :: Menu (Either HandBuilder Hand)
initialMenu = MenuItem (prompt initialBuilder) (Left initialBuilder)

expand :: (v -> [Menu w]) -> Menu v -> Menu w
expand f (MenuItem s v) = Menu s (f v)
expand f (Menu s ms) = Menu s (L.map (expand f) ms)

expandAll :: Menu (Either HandBuilder Hand) -> Menu Hand
expandAll = expand (either (fmap expandAll . selectMenuItems) ((: []) . menuItem))

selectAll :: HandBuilder -> [(String, Either HandBuilder Hand)]
selectAll hb = zipWith (curry (fmap (selectOption hb))) (optionList hb) [1 ..]

selectMenuItems :: HandBuilder -> [Menu (Either HandBuilder Hand)]
selectMenuItems = menuItems . selectAll

formatMenu :: Menu a -> String
formatMenu = unlines . formatMenu'

formatMenu' :: Menu a -> [String]
formatMenu' (MenuItem s v) = [s]
formatMenu' (Menu s ts) = s : fmap ('\t' :) (ts >>= formatMenu')