import Data.List as L
import Control.Monad as Mnd

chordNotes :: Integral n => [n] -> n -> [n]
chordNotes [] open = []
chordNotes as open = 
    let xs@(n:ns) = sort $ fmap ((`mod` 12) . subtract open) as
    in xs ++ [n+12]

rootNotes :: Integer -> [Integer]
rootNotes = chordNotes [0,4,7]

secondNotes :: Integer -> [Integer]
secondNotes = chordNotes [2,5,9]

thirdNotes :: Integer -> [Integer]
thirdNotes = chordNotes [4,7,11]

fourthNotes :: Integer -> [Integer]
fourthNotes = chordNotes [5,9,0]

fifthNotes :: Integer -> [Integer]
fifthNotes = chordNotes [7,11,2]

sixthNotes :: Integer -> [Integer]
sixthNotes = chordNotes [9,0,4]

seventhNotes :: Integer -> [Integer]
seventhNotes = chordNotes [11,2,5]

rootChords :: [Integer] -> [[Integer]]
rootChords = fmap rootNotes

secondChords :: [Integer] -> [[Integer]]
secondChords = fmap secondNotes

thirdChords :: [Integer] -> [[Integer]]
thirdChords = fmap thirdNotes

fourthChords :: [Integer] -> [[Integer]]
fourthChords = fmap fourthNotes

fifthChords :: [Integer] -> [[Integer]]
fifthChords = fmap fifthNotes

sixthChords :: [Integer] -> [[Integer]]
sixthChords = fmap sixthNotes

seventhChords :: [Integer] -> [[Integer]]
seventhChords = fmap seventhNotes

printChords :: [[Integer]] -> IO ()
printChords = void . traverse print

stdTuning key = fmap ((`mod` 12) . subtract key) [4,11,7,2,9,4]

csus4add9Tuning = [2,0,4,0,5,0]