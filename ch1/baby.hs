-- Function Basics
doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
  then x
  else x*2

-- List Basics

-- concatMe [1] [2] ---> [1,2]
-- concatMe ['D', 'o'] ['u', 'g'] ---> "Doug"
concatMe l c = l ++ c

-- consMe 1 [2,3] ---> [1,2,3]
-- consMe 'H' "ello" ---> "Hello"
consMe h l = h:l

-- isInList 'e' "Hello" ---> True
-- isInList 'z' "Hello" ---> False
-- isInList 1 [3,2,1] ---> True
isInList e l = e `elem` l

boomBangs xs = [if x < 10 then "Boom" else "Bang" | x <- xs, odd x]

unit a b = (a,b)
