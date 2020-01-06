import Data.List as L

chordNotes :: Integral n => [n] -> n -> [n]
chordNotes [] open = []
chordNotes as open = 
    let xs@(n:ns) = sort $ fmap ((`mod` 12) . (subtract open)) as
    in xs ++ [n+12]

rootNotes :: Integer -> [Integer]
rootNotes = chordNotes [0,4,7]

secondNotes :: Integer -> [Integer]
secondNotes = chordNotes [2,5,9]

thirdNotes = chordNotes [4,7,11]

fourthNotes = chordNotes [5,9,0]

fifthNotes = chordNotes [7,11,2]

sixthNotes = chordNotes [9,0,4]

seventhNotes = chordNotes [11,2,5]