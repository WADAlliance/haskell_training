-- 17.9 Chapter Exercises

--1. 

--instance Applicative [] where
    pure :: a -> [a]
    (<*>) :: [(a -> b)] -> [a] -> [b]


--2.

pure :: a -> IO (a)
(<*>) :: IO (a -> b) -> IO (a) -> IO (b)

--3.
--(,) a 
-- (,) a b ----> (Int , String)
pure :: a -> (s, a)
(<*>) :: (s, (a -> b)) -> (s, a) -> (s,b)

--4. -- Type
(->) e
-- Methods
pure :: a -> (e -> a)
(<*>) :: e -> (a -> b) -> (e -> a) -> (e -> b)

-- Write instances for the following datatypes. Confused?
--Write out what the type should be. Use the checkers library
--to validate the instances.
--1. 
data Pair a = Pair a a deriving Show
instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure x = Pair x x
    --  <*> :: Paire (a->b) -> Pair a -> Pair b
    Pair f g <*> Pair x y = Pair (f x) (g y) 

-- 2. This should look familiar.
data Two a b = Two a b
instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)
instance Applicative (Two a) where
    pure x = Two a x
    Two a f <*> Two a x = Two a (f x)
-- 3. 
data Three a b c = Three a b c
-- 4.
data Three' a b = Three' a b b
-- 5. 
data Four a b c d = Four a b c d
-- 6. 
data Four' a b = Four' a a a b







