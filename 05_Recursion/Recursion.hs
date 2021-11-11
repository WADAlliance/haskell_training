--8.6 Chapter Exercises

---

-- Recursion


--1- 

    stepsDividedBy :: (Integral a) => a->a->a
    stepsDividedBy dividande diviseur
        | dividande < diviseur = 0
        | otherwise = 1 + stepsDividedBy (dividande-diviseur) diviseur


--2-


    sumN ::  (Eq a, Num a) => a -> a
    sumN a
        | a == 0 = 0
        | otherwise = a + sumN (a-1)

--3-

    multiplyByAddition :: (Integral a) => a -> a -> a
    multiplyByAddition a b
        | b == 0 = 0
        | otherwise = a + multiplyByAddition a (b-1)


-- Fixing dividedBy

    data DividedResult = 
          Result Integer 
        | DividedByZero 
        deriving Show

    instance Num DividedResult where
        (+) DividedByZero _ = DividedByZero
        (+) _ DividedByZero = DividedByZero
        (+) (Result a ) (Result b) = Result (a+b)
        fromInteger a = Result a


    stepsDividedBy' :: (Integral a) => a->a->DividedResult
    stepsDividedBy' dividande diviseur
        | diviseur == 0 = DividedByZero
        | abs dividande < abs diviseur = Result 0
        | otherwise = stepsDividedBy' (abs dividande-abs diviseur) (abs diviseur) + 1


    dividedBy :: (Integral a, Eq a) => a -> a -> (DividedResult, DividedResult)
    dividedBy a b
        | b == 0 = (DividedByZero, DividedByZero)
        | otherwise = (Result (toInteger (fst  res)), Result (toInteger (snd res)))
            where res = divMod  a b


-- McCarthy 91 function

    mc91 :: Integer -> Integer
    mc91 x 
        | x > 100 = x - 10
        | otherwise = mc91(mc91(x+11))


-- Numbers into words

    digitToWord :: Int -> String
    digitToWord n 
        | n == 0 = "zero"
        | n == 1 = "one"
        | n == 2 = "two"
        | n == 3 = "three"
        | n == 4 = "four"
        | n == 5 = "five"
        | n == 6 = "six"
        | n == 7 = "seven"
        | n == 8 = "heigt"
        | n == 9 = "nine"
        | otherwise = error "Something ent rong"


    digits :: Int -> [Int]
    digits n 
        | n < 10 = [n]
        | otherwise =  digits qq ++ [rr]
            where  (qq, rr) = divMod n 10 


    wordNumber :: Int -> String
    wordNumber n = 
        let digitsOfn = digits n 
            listord = map digitToWord digitsOfn
        in concat $ intersperse "-" listord 


