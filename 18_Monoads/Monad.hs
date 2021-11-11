{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

import Data.Monoid
import Control.Monad
import Data.Traversable
--import Control.Monad.Writer
--18.7 Chapter Exercises
{-
Write Monad instances for the following types. Use the QuickCheck
properties we showed you to validate your instances.
-}

--1.

data Nope a = NopeDotJpg
instance Functor Nope where
    fmap f x = NopeDotJpg
instance Applicative Nope where
    pure x = NopeDotJpg
    nopeF <*> nopeX = NopeDotJpg
instance Monad Nope where
    return x = NopeDotJpg
    nopeX >>= nopeF = NopeDotJpg


--2. 
data PhhhbbtttEither b a =
      Gauche a
    | Droite b

instance  Functor (PhhhbbtttEither x) where
    fmap f (Gauche u) = Gauche $ f u
    fmap f (Droite v) = Droite v
instance Applicative (PhhhbbtttEither x) where
    pure u = Gauche u
    (Gauche f) <*> (Gauche u) = Gauche $ f u
    (Gauche f) <*> (Droite u) = Droite u
    --(Droite f) <*> (Gauche u) = Gauche (f u)
    --(Droite f) <*> (Droite u) = Droite u
instance Monad (PhhhbbtttEither x) where
    return  = Gauche 
    Gauche u >>= f = f u
    Droite u >>= f = Droite u


-- 3. Write a Monad instance for Identity.

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity $ f x
instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity x) = Identity $ f x
instance Monad Identity where
    return = pure
    (Identity x) >>= f = f x



--4. This one should be easier than the Applicative instance
--was. Remember to use the Functor that Monad requires, then
--see where the chips fall.
data List a =
      Nil
    | Cons a (List a)
instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)
instance Semigroup a => Semigroup (List a) where
    Nil <> xs = xs
    --xs <> Nil = xs
    (Cons x xs) <> y = Cons x (xs <> y)
    

instance Monoid a => Monoid (List a) where
    mempty = Nil
    mappend Nil xs = xs
    mappend xs Nil = xs
    mappend (Cons x xs) ys = Cons x (mappend xs ys)


mymappend :: List x -> List x -> List x
mymappend Nil ys = ys
mymappend ys Nil = ys
mymappend (Cons x xs) ys = Cons x (mymappend xs ys)


instance Applicative List where
    pure x = Cons x Nil
    --(<*>) :: forall a b. List (a -> b) -> List a  -> List b 
    Nil <*> x = Nil
    fs <*> Nil = Nil
    (Cons f fs) <*> (Cons x xs) = mymappend ((Cons (f x) (fmap f xs)))  ((fs <*> (Cons x xs))) -- (fs <*> (Cons x xs))  --(join (fmap f xs))
                                          
 
instance Monad List where 
    return  = pure
    Nil >>= f = Nil
    (Cons x xs) >>= f = myjoin (Cons (f x) (fmap f xs))
                          where  myjoin :: List (List a) -> List a 
                                 myjoin xs = myfoldr mymappend Nil xs 
                                            where   myfoldr :: (List a -> List a -> List a) -> List a -> List (List a) -> List a 
                                                    myfoldr f acc Nil = acc
                                                    myfoldr f acc (Cons u us) =  myfoldr f (f acc u) us


--Write the following functions using the methods provided
--by Monad and Functor. Using stuﬀ like identity and composition
--is fine, but it has to typecheck with types provided.

--1.

j :: Monad m => m (m a) -> m a
j = join



--2.

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap


--3.

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

--4.

a :: Monad m => m a -> m (a -> b) -> m b

a = flip (<*>)   


--5. You’ll need recursion for this one.

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh = flip traverse

--6.

flipType :: (Monad m) => [m a] -> m [a]

flipType = flip meh $ id



--- Accumulateur

-- [1, 2, 3, 4] ----> [1, 3, 6, 10]
{-
accumulateur :: [Int] -> [Int]
accumulateur xs = foldl f [] xs
                    where f :: [Int] -> Int -> [Int]
                          f acc y 
                            | acc == [] = [y]
                            | otherwise = acc ++ [((last acc) + y)]

-}

instance Functor (Writer w) where
    fmap f (Writer (a,v)) = Writer (f a, v)

instance (Monoid w) => Applicative (Writer w) where
    pure x = Writer (x, mempty)
    Writer (f, v) <*> Writer (a,v') = Writer (f a, v<>v')

newtype Writer w a = Writer { runWriter :: (a, w) } deriving Show
instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')


accumulateur :: [Int] -> Writer [String] [Int]
accumulateur xs = foldl f (Writer ([], [])) xs
                    where f :: Writer [String] [Int] -> Int -> Writer [String] [Int]
                          f (Writer (acc,v)) y 
                            | acc == [] = Writer ([y], [show [y]]) 
                            | otherwise = Writer (acc ++ [((last acc) + y)], [show $ acc ++ [((last acc) + y)]]) 


naim :: Writer [String] [Int]
naim = do
    r1 <- accumulateur [2, 6]

    r2 <- accumulateur [3, 5]

    r3 <- accumulateur [4, 2]
    
    return  r1



 