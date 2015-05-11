lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!!!"
lucky x = "Ya blew it"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe x = "Not between 1 and 5"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

addTuples :: (Num a) => [(a, a)] -> [a]
addTuples [] = []
addTuples xs = [x + y | (x,y) <- xs]

head' :: [a] -> a
head' [] = error "Cannot call head on empty list - ya blew it"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
  | bmi <= 18.5 = "Underweight"
  | bmi <= 25.0 = "Right on"
  | bmi <= 30.0 = "Overweight"
  | otherwise = "Obese"

bmiCalc :: (RealFloat a) => a -> a -> String
bmiCalc weight height
  | weight / height ^ 2 <= 18.5 = "Underweight"
  | weight / height ^ 2 <= 25.0 = "Right on"
  | weight / height ^ 2 <= 30.0 = "Overweight"
  | otherwise = "Obese"

bmiWhere :: (RealFloat a) => a -> a -> String
bmiWhere weight height
  | bmi <= skinny = "Underweight"
  | bmi <= normal = "Right on"
  | bmi <= fat = "Overweight"
  | otherwise = "Obese"
  where bmi = weight / height ^ 2
        skinny = 18.5
        normal = 25.0
        fat = 30.0

bmiWhere' :: (RealFloat a) => a -> a -> String
bmiWhere' weight height
  | bmi <= skinny = "Underweight"
  | bmi <= normal = "Right on"
  | bmi <= fat = "Overweight"
  | otherwise = "Obese"
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

calcBmisLet :: (RealFloat a) => [(a, a)] -> [a]
calcBmisLet xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

calcBmisFatOnly :: (RealFloat a) => [(a, a)] -> [a]
calcBmisFatOnly xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]  
