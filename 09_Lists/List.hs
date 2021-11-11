
import Data.Char ( ord, chr, isUpper, toUpper )
import Text.ParserCombinators.ReadPrec (prec)
import Data.List ()
--9.12 Chapter Exercises

---

--Data.Char

--
--
--

--2- 


filterUpperCase :: [Char] -> [Char]
filterUpperCase  = filter isUpper


--3-


capitalizeFirst :: [Char] -> [Char]
capitalizeFirst [] = [] 
capitalizeFirst (x:xs) = toUpper x : xs


--4-


capitalizeString :: [Char] -> [Char]
capitalizeString [] =  []  
capitalizeString (x:xs) = toUpper x : capitalizeString xs


--5-

capitalizeFirstRet :: [Char] -> Char
capitalizeFirstRet [] = error "Empty List" 
capitalizeFirstRet (x:xs) = toUpper x 

--6-

    --with param

capitalizeFirstRet' :: [Char] -> Char
capitalizeFirstRet' [] = error "Empty List" 
capitalizeFirstRet' xs = toUpper $ head xs

    --Point free
capitalizeFirstRet'' :: [Char] -> Char
capitalizeFirstRet'' = toUpper . head 


 --Writing your own standard functions

 

--1-


myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs 


--2-

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs


--3-

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False 
myElem x (y:xs) = x == y || myElem x xs

myElem1 :: Eq a => a -> [a] -> Bool 
myElem1 a = foldr step False
    where step x y = x == a || y 


myElem2 :: Eq a => a -> [a] -> Bool 
myElem2 a = any (==a)


--4-


myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]


--5-


squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs


--6-

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f  =  squish . map f


--7-


squishAgain :: [[a]] -> [a]
squishAgain = squishMap id


--8-


myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "Somthing ent rong"
myMaximumBy f (x:xs) = go f x xs
    where 
        go _ mx [] = mx
        go fonc mx (y:ys) 
                | fonc mx y == LT = go fonc y ys
                | fonc mx y == GT = go fonc mx ys
                | otherwise = go fonc mx ys
          
--9-


myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "Somthing ent rong"
myMinimumBy f (x:xs) = go f x xs
    where 
        go _ mx [] = mx
        go fonc mx (y:ys) 
                | fonc mx y == LT = go fonc mx ys
                | fonc mx y == GT = go fonc y ys
                | otherwise = go fonc mx ys


