
--20.6 Chapter Exercises
--Write Foldable instances for the following datatypes.

-- foldr :: (a -> b -> b) -> b -> t a -> b
-- foldMap :: (a -> b) -> t a -> b
--1. 
data Constant a b = Constant b
instance Foldable (Constant a) where
    foldr f acc (Constant x) = f x acc 
    foldMap f (Constant x)   = f x


--2. 
data Two a b = Two a b
instance Foldable (Two a) where
    foldr f acc (Two x y) = f y acc 
    foldMap f (Two x y)   = f y

--3. 
data Three a b c = Three a b c
instance Foldable (Three a b) where
    foldr f acc (Three x y z) = f z acc 
    foldMap f (Three x y z)    = f z
--4. 
data Three' a b = Three' a b b
instance Foldable (Three' a) where
    --foldr f acc (Three' x y z)  =  (f y acc) <>  (f z acc)
    foldMap f  (Three' x y z)   =  (f y) <> (f z)
--5. 
data Four' a b = Four' a b b b
instance Foldable (Four' a) where
    --foldr f acc (Four' x y z u)  =  (f y acc) <>  (f z acc) <> (f u acc)
    foldMap f  (Four' x y z u)   =  (f y) <> (f z) <> (f u)










