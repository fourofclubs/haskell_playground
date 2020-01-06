import Data.Map as M
import Data.List as L
import Data.List.Split as S
import Data.Tuple
import Control.Monad
import Data.Text
import Data.Maybe

main = do
    ms <- loadMatches
    rinksContents <- readFile "rinks.txt"
    start <- getLine
    let rm = rinksMap rinksContents
    let rinksLookup = (fromMaybe "rink not found") . (flip M.lookup rm)
    putStrLn . either id id $ (fmap (formatTree (formatMatch rinksLookup)) . join . fmap (fullMatchTree start) $ ms)

data Match = Match {matchId :: String, time :: String, rink :: Int, win :: Maybe String, loss :: Maybe String} 
    deriving (Eq, Read)
instance Show Match where
    show m = matchId m ++ ", " ++ time m ++ ", " ++ (show . rink $ m)
data Tree a = Tree Match (Tree a, Tree a) | Leaf (Maybe a) deriving (Show, Eq, Read)
instance Functor Tree where
    fmap f (Tree m (t1, t2)) = Tree m (fmap f t1, fmap f t2)
    fmap f (Leaf ma) = Leaf (fmap f ma)

loadMatches :: IO (Either String (Map String Match))
loadMatches = do
    contents <- readFile "mca2018.txt"
    return (matches contents)

stripS :: String -> String
stripS = unpack . strip . pack

joinTree :: Tree (Tree a) -> Tree a
joinTree (Tree m (t1, t2)) = Tree m (joinTree t1, joinTree t2)
joinTree (Leaf Nothing) = Leaf Nothing
joinTree (Leaf (Just mt)) = mt

fullMatchTree :: String -> Map String Match -> Either String (Tree String)
fullMatchTree startMatch m = let initial = lookupE m startMatch in
    (fmap matchTree initial) >>= (expandTree (fmap matchTree . lookupE m))

matches :: String -> Either String (Map String Match)
matches s = let ls = fmap (stripS) (Prelude.lines s) in
    let ps = fmap (S.splitOn ",") ls in
    let ms = sequence (fmap match ps) in
    fmap matchMap ms

finalMatch id time rink = Match id time rink Nothing Nothing
knockoutMatch id time rink win = Match id time rink (Just win) Nothing
normalMatch id time rink win loss = Match id time rink (Just win) (Just loss)

match :: [String] -> Either String Match
match [id, time, rink] = Right (finalMatch id time (read rink))
match [id, time, rink, win] = Right (knockoutMatch id time (read rink) win)
match [id, time, rink, win, loss] = Right (normalMatch id time (read rink) win loss)
match ps = Left ("Incorrect parameter list size to create match: " ++ show ps)

headValue :: Map a b -> b
headValue m = snd . Prelude.head . toList $ m

sequenceTree :: Tree (Either e a) -> Either e (Tree a)
sequenceTree (Tree m (t1, t2)) = mergeTree m (sequenceTree t1) (sequenceTree t2)
sequenceTree (Leaf Nothing) = Right . Leaf $ Nothing
sequenceTree (Leaf (Just (Left e))) = Left e
sequenceTree (Leaf (Just (Right a))) = Right . Leaf . Just $ a

expandMatch :: Map String Match -> String -> Either String (Tree String)
expandMatch ms id = fmap (matchTree) (lookupE ms id)

mergeTree :: Match -> Either e (Tree a) -> Either e (Tree a) -> Either e (Tree a)
mergeTree m (Left e) _ = Left e
mergeTree m _ (Left e) = Left e
mergeTree m (Right t1) (Right t2) = Right (Tree m (t1, t2))

expandTree :: (a -> Either e (Tree a)) -> Tree a -> Either e (Tree a)
expandTree f (Tree m (t1, t2)) = mergeTree m (expandTree f t1) (expandTree f t2)
expandTree f (Leaf Nothing) = Right . Leaf $ Nothing
expandTree f (Leaf (Just a)) = (f a) >>= (expandTree f)

matchTree :: Match -> Tree String
matchTree m = Tree m (Leaf (win m), Leaf (loss m))

matchMap :: [Match] -> Map String Match
matchMap ms = M.fromList (fmap (swap . fmap matchId) (Prelude.zip ms ms))

lookupE :: (Ord a, Show a) => Map a b -> a -> Either String b
lookupE m id = case M.lookup id m of
    Nothing -> Left ("Key " ++ show id ++ " missing.")
    Just a -> Right a

expandByLookup :: (Show a, Ord a) => Map a b -> Tree a -> Either String (Tree b)
expandByLookup m = sequenceTree . fmap (lookupE m)

formatTree :: (Show a) => (Match -> String) -> Tree a -> String
formatTree fm t = Prelude.unlines (formatTree' fm t)

formatTree' :: (Show a) => (Match -> String) -> Tree a -> [String]
formatTree' fm (Tree m (t1, t2)) = [fm m] ++ (fmap ("\t" ++) (formatTree' fm t1 ++ formatTree' fm t2))
formatTree' fm (Leaf Nothing) = []
formatTree' fm (Leaf (Just a)) = [show a]

formatMatch :: (Int -> String) -> Match -> String
formatMatch rinks m = (show m) ++ "(" ++ (rinks . rink $ m) ++ ")"

rinksMap :: String -> Map Int String
rinksMap s = L.foldl (M.union) (M.empty) (fmap (club) (Prelude.lines s))

club :: String -> Map Int String
club s = case Prelude.words s of
    name:n1:n2:[] -> M.fromList (fmap (\n -> (n,name)) [read n1 .. read n2])
    _ -> error ("Could not parse rinks:" ++ s)