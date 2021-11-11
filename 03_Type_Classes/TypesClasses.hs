--6.14 Chapter Exercises

-- Does it type check?

---

--Type-Kwon-Do Two: Electric Typealoo

--1- 

    chk :: Eq b => (a -> b) -> a -> b -> Bool
    chk f x y = f x == y

--2-


    arith :: Num b => (a -> b) -> Integer -> a -> b
    arith f _ = f

    