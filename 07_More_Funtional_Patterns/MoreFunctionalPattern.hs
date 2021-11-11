--7.11 Chapter Exercises

---

-- Let''s write code


--1- 

    tensDigit :: Integral a => a -> a -> a
    tensDigit b x = d
        where xLast = x `div` b
              d = xLast `mod` 10


    tensDigit' :: Integral a => a -> a -> a
    tensDigit' b x = snd $ divMod (fst $ divMod x b) 10

--2- 

    foldBool3 :: a -> a -> Bool -> a
    foldBool3 a b c =
        case c of
            True -> a
            False -> b

    foldBool3' :: a -> a -> Bool -> a
    foldBool3' a b c
        | c  = a
        | otherwise  = b

--3-


    g :: (a -> b) -> (a, c) -> (b, c)
    g fab (a, c) = (fab a, c)


--5-

    roundTrip :: (Show a, Read a) => a -> a
    roundTrip = read . show



