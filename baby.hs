doubleMe x = x * 2
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100 then x else doubleMe x
doubleSmallNumber' x = doubleSmallNumber x + 1

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

initials :: String -> String -> String
initials firstname@(f:irstname) lastname@(l:astname) = firstname ++ " " ++ lastname ++ ": " ++ [f] ++ ". " ++ [l] ++ "."
initials _ _ = "That's not a name!"