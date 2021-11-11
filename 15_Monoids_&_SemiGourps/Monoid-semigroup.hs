--import Data.Semigroup
--import Data.List
data Trivial = Trivial deriving (Eq, Show)


instance Semigroup Trivial where
    Trivial <> Trivial = Trivial


newtype Identity a = Identity a
instance Semigroup a => Semigroup (Identity a) where
    Identity a <> Identity b = Identity (a<>b)


data Two a b = Two a b
instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    Two a b <> Two c d = Two (a<>c) (b<>d)


data Three a b c = Three a b c
instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    Three a b c <> Three x y z  = Three (a<>x) (b<>y) (c<>z)



newtype BoolConj = BoolConj Bool 

instance Semigroup BoolConj where
    BoolConj a <> BoolConj b = BoolConj (a && b)


newtype BoolDisj = BoolDisj Bool deriving Show
instance Semigroup BoolDisj where
    BoolDisj a <> BoolDisj b = BoolDisj (a || b)


data Or a b =
      Fst a
    | Snd b
instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
    Fst x1 <> Fst x2 = Fst x2
    Fst x <> Snd y = Snd y
    Snd y <> Fst x = Snd y
    Snd y1 <> Snd y2 = Snd y1 


newtype Combine a b = Combine { unCombine :: (a -> b) }
instance Semigroup b => Semigroup (Combine a b) where
    x <> y = Combine $ \n -> (unCombine x) n <>  (unCombine y) n


newtype Comp a = Comp { unComp :: (a -> a) }
instance Semigroup a => Semigroup (Comp a) where
    x <> y = Comp $ \n -> (unComp x) n <>  (unComp y) n



data Validation a b = Failure a | Success b deriving (Eq, Show)
instance Semigroup a => Semigroup (Validation a b) where
    Failure x <> Failure y = Failure (x<>y)
    Failure x <> Success y = Success y
    Success x <> Failure y = Success x
    Success x <> Success y = Success x



main = do
    let failure :: String -> Validation String Int
        failure = Failure
        success :: Int -> Validation String Int
        success = Success
    print $ success 1 <> failure "blah"
    print $ failure "woot" <> failure "blah"
    print $ success 1 <> success 2
    print $ failure "woot" <> success 2




-- Monoid exercises

-- 1. 

instance Monoid Trivial where 
    mempty = Trivial
    mappend = (<>)


--2.

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)


--3. 

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

--4.


instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)


--5.

instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend = (<>)


--6.

instance (Monoid b) =>  Monoid (Combine a b) where
    mempty = Combine $ \x -> mempty
    mappend = (<>)

--myFunc = Combine $ \n -> Sum (n + 1)
--main1 = unCombine (mappend myFunc mempty) $ 1

--  x <> y = Combine $ \n -> (unCombine x) n <>  (unCombine y) n


--7.

instance (Monoid a) =>  Monoid (Comp a) where
    mempty = Comp $ \x -> mempty
    mappend = (<>)

--8.

newtype Mem s a = Mem { runMem :: s -> (a,s) }
instance Semigroup a => Semigroup (Mem s a) where
   x<>y = Mem $ \s -> ((fst $ (runMem x) s) <> (fst $ (runMem y) s), snd $ (runMem x) $ (snd $ (runMem y) s))
--x<>y = Mem $ \s -> ((runMem x) s) <> ((runMem y) s)
instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \s->(mempty, s) 
    mappend = (<>)

f' = Mem $ \s -> ("hi", s + 1)
main2 = do
    let rmzero = runMem mempty 0
        rmleft = runMem (f' <> mempty) 0
        rmright = runMem (mempty <> f') 0
    print $ rmleft
    print $ rmright
    print $ (rmzero :: (String, Int))
    print $ rmleft == runMem f' 0
    print $ rmright == runMem f' 0





data List a =
      Nil
    | Blah a (List a)


