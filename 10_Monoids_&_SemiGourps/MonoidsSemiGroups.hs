
data Trivial = Trivial deriving (Eq, Show)

--insta

newtype Identity a = Identity a

instance Semigroup a => Semigroup (Identity a) where             
    Identity a <> Identity b = Identity (a <> b)

