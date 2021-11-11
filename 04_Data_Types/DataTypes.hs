--4.9 Chapter Exercises



--8- 

    isPalindrome :: (Eq a ) => [a] -> Bool
    isPalindrome xs = xs == rvrs xs
            where rvrs ys = foldl (flip :) [] ys

--9-

    myAbs :: Integer -> Integer
    myAbs a = if a < 0
            then negate a
            else a

--10- 

    f :: (a, b) -> (c, d) -> ((b, d), (a, c))
    f x y = ((snd x, snd y), (fst x, fst y))


--Correcting syntax

--1 

    f :: [a] -> Int
    f xs = w `x` 1
        where w = length xs

--2  

    \x -> x

--3

    h::(x,y)->x
    h (x, _) = x

--




