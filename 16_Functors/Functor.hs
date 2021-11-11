
--16.17 Chapter exercises

--1.

-- No the kind of Bool *

--2. Yes the kind of BoolAndSomethingElse is * -> *

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
import GHC.Arr

data BoolAndSomethingElse a = False' a | True' a
instance Functor BoolAndSomethingElse where
    fmap :: (a -> b) -> BoolAndSomethingElse a -> BoolAndSomethingElse b 
    fmap f (False' x) = False' (f x)
    fmap f (True' x) = True' (f x)



--3.
-- Yes the kind of BoolAndMaybeSomethingElse a is * -> *
data BoolAndMaybeSomethingElse a = Falsish | Truish a
instance Functor BoolAndMaybeSomethingElse where
    fmap :: (a -> b) -> BoolAndMaybeSomethingElse a -> BoolAndMaybeSomethingElse b 
    fmap f Falsish = Falsish
    fmap f (Truish x) = Truish (f x)

--4.
-- No : the kind of Mu is (* -> *) -> *


--5.

-- No the kind of D is *

data D a = D (Array Word Word) Int a
instance Functor D where
    fmap :: (a -> b) -> D a -> D b 
    fmap f (D (Array w1 w2 w3 w4) i x) = D (Array w1 w2 w3 w4) i (f x)


{-
Rearrange the arguments to the type constructor of the
datatype so the Functor instance works.
 -}

--1.

data Sum b a =
      First a
    | Second b
instance Functor (Sum e) where
    fmap f (First a) = First (f a)
    fmap f (Second b) = Second b



--2.

data Company a c b =
      DeepBlue a c
    | Something b
instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c


--3. 
data More b a =
      L a b a
    | R b a b
    deriving (Eq, Show)
instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'


-- Write Functor instances for the following datatypes.

-- 1. 
data Quant a b =
      Finance
    | Desk a
    | Bloor b

instance Functor (Quant x) where
    fmap f Finance = Finance
    fmap f (Desk y)  = Desk y
    fmap f (Bloor y) = Bloor (f y)

--2.

data K a b = K a
instance Functor (K z) where
    fmap f (K x) = K x 


--3. 

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
--newtype K a b = K a
-- should remind you of an
-- instance you've written before
{-
instance Functor (Flip K a) where
    fmap :: (a' -> b') -> Flip K a a' -> Flip K a b' 
    fmap f (Flip (g x y)) =  Flip (g x (f y))
-}

-- 4. 
data EvilGoateeConst a b = GoatyConst b
instance Functor ( EvilGoateeConst a) where
    fmap f (GoatyConst x) = GoatyConst (f x)


-- 5. Do you need something extra to make the instance work?
data LiftItOut f a = LiftItOut (f a)
instance Functor f => Functor (LiftItOut f) where
    fmap :: (a -> b) -> LiftItOut f a -> LiftItOut f b 
    -- fmap :: (a -> b) -> f a -> f b 
    fmap g (LiftItOut f) = LiftItOut $ fmap g f


--6.

data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap :: (a -> b) -> Parappa f g a -> Parappa f g b 
    fmap h (DaWrappa f g) = DaWrappa  (fmap h f) (fmap h g)


--7.
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance (Functor g) => Functor (IgnoreOne f g a) where
    fmap :: (a' -> b') -> IgnoreOne f g a a' -> IgnoreOne f g a b' 
    fmap h (IgnoringSomething fa gb) = IgnoringSomething fa (fmap h gb)

-- 8.
data Notorious g o a t = Notorious (g o) (g a) (g t)
instance (Functor g) => Functor (Notorious  g o a) where
    fmap :: (a' -> b') -> Notorious  g o a a' -> Notorious  g o a b' 
    fmap h (Notorious go ga gt) = Notorious go ga  (fmap h gt)



--9.

data List a =
      Nil
    | Cons a (List a)
instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

--10.

data GoatLord a =
      NoGoat
    | OneGoat a
    | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
    fmap f NoGoat = NoGoat
    fmap f (OneGoat x) = OneGoat (f x)
    fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z) 


--11

data TalkToMe a =
      Halt
    | Print String a
    | Read (String -> a)
instance Functor TalkToMe where
    fmap f Halt = Halt
    fmap f (Print x y) = Print x (f y)
    fmap f (Read g) = Read (f.g)
























