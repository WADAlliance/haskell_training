--10.10 Chapter Exercises

---

--Warm up and review

stops :: [Char]
stops = "pbtdkg"
vowels :: [Char]
vowels = "aeiou"

---1.a-

svtCombinations :: [Char] -> [Char] -> [(Char, Char, Char)]
svtCombinations stp vwls  = [(s1, v, s2) | s1 <- stp, s2 <- stp, v <- vwls] 

svtCombinations' :: [Char] -> [Char] -> [(Char, Char, Char)]
svtCombinations' stp vwls = do
    s1 <- stp
    v <- vwls
    s2 <- stp
    return (s1, v, s2)


--1.b-

svtCombinations'' :: [Char] -> [Char] -> [(Char, Char, Char)]
svtCombinations'' stp vwls  = [('p', v, s2) | s2 <- stp, v <- vwls] 

--1.c-


--2-


--3-


seekritFunc :: Fractional a => String -> a
seekritFunc x = 
    fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))
 


--Rewriting functions using folds

--1- 

myOr :: [Bool] -> Bool 
myOr = foldr f False
            where f a b = a || b

--2-


myAny :: (a -> Bool) -> [a] -> Bool 
myAny f = foldr g False
    where g x y = f x || y 

--3-

myElem :: Eq a => a -> [a] -> Bool 
myElem a = foldr step False
    where step x y = x == a || y 


myElem' :: Eq a => a -> [a] -> Bool 
myElem' a = any (==a)


--4-


myReverse :: [a] -> [a]
myReverse = foldl step []
    where step a b = b : a

--5-

myMap :: (a->b) -> [a] -> [b]
myMap f = foldr step []
    where step x y = f x : y  

--6-

myFilter :: (a -> Bool)-> [a] -> [a]
myFilter f = foldr step []
    where step x y = if f x then x : y else y

--7-

squish :: [[a]] -> [a]
squish = foldr step []
    where step x y = x ++ y