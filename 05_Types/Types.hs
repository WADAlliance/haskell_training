--5.8 Chapter Exercises

-- Does it compile?

---

--Given a type, write a function

--0- 

    myFunc :: (x -> y) -> (y -> z) -> c -> (a, x) -> (a, z)
    myFunc xToY yToZ _ (a, x) = (a, yToZ $ xToY x )

--1-

    i :: a -> a
    i = \ x -> x

--2-

    c :: a -> b -> a
    c x _ = x 

--6 

    co :: (b -> c) -> (a -> b) -> a -> c
    co f g = f . g 

--7 

    a :: (a -> c) -> a -> a
    a _ x = x 

--8 

    a' :: (a -> b) -> a -> b 
    a' f = f 

-- Fix it

--1-

    fstString :: [Char] -> [Char]
    fstString x = x ++ " in the rain"

    sndString :: [Char] -> [Char]
    sndString x = x ++ " over the rainbow"

    sing :: [Char]
    sing = if x > y then sndString $ fstString x else fstString $ sndString y
        where x = "Singin"
              y = "Somewhere"

--3- 

    main :: IO ()
    main = do
        print ((1 + 2)::Integer)
        putStrLn $ show (10 :: Integer)
        print (negate (negate (1::Integer)))
        print ((+) (0::Integer) blah)
            where blah = negate 1

--Type-Kwon-Do


--1- 

    f :: Int -> String
    f = undefined
    g :: String -> Char
    g = undefined
    h :: Int -> Char
    h = g.f

--2-

    data A
    data B
    data C
    q :: A -> B
    q = undefined
    w :: B -> C
    w = undefined
    e :: A -> C
    e = w.q

--3-

    data X
    data Y
    data Z
    xz :: X -> Z
    xz = undefined
    yz :: Y -> Z
    yz = undefined
    xform :: (X, Y) -> (Z, Z)
    xform (a,b)=(xz a, yz b)

--4-

    munge :: (x -> y) -> (y -> (w, z)) -> x -> w
    munge u v a = fst (v $ u a)
