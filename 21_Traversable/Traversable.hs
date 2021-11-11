{-# LANGUAGE InstanceSigs #-}


--21.12 Chapter Exercises

-- Traversable instances

{-
Write a Traversable instance for the datatype provided, filling
in any required superclasses. Use QuickCheck to validate your
instances.
-}

--Identity
--Write a Traversable instance for Identity.
-- 
newtype Identity a = Identity a deriving (Eq, Ord, Show)
instance Functor Identity where
    fmap f (Identity x) = Identity $ f x
instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity x) = Identity $ f x
instance Foldable Identity where
    foldMap f (Identity x) = f x

instance Traversable Identity where
    traverse :: (Applicative f) => (a -> f b) -> Identity a -> f (Identity b)
    traverse g (Identity x) = fmap Identity (g x)


--Constant


newtype Constant u v = Constant { getConstant :: u}
instance Functor (Constant u) where                                
    --fmap :: (a -> b) -> (Constant u) a -> (Constant u) b
    fmap f (Constant x) = Constant x {-myidentity x 
                where  myidentity :: (Constant u) a -> (Constant u) b
                       myidentity (Constant x) = (Constant $ id x)   -}                           
instance (Monoid a) => Applicative (Constant a) where
    pure x = Constant mempty
    _ <*> (Constant x) = Constant x
instance Foldable (Constant t) where
    foldMap :: (Monoid b) => (a -> b) -> Constant t a -> b
    foldMap f (Constant x) = mempty
instance Traversable (Constant t) where
    traverse :: (Applicative f) => (a -> f b) -> (Constant t) a -> f ((Constant t) b)
    traverse g (Constant x) = pure $ Constant x
--instance Traversable Identity where
   -- traverse :: (Applicative f) => (a -> f b) -> Identity a -> f (Identity b)
    --traverse g (Identity x) = fmap Identity (g x)




