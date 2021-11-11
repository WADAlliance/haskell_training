--12.5 Chapter Exercises

---

-- String processing


--1- 


    notThe :: String -> Maybe String
    notThe a
            | a == "the" = Nothing
            | otherwise = Just a


--2-


    countTheBeforeVowel :: String -> Integer
    countTheBeforeVowel str = mycount $ words str
                                    where mycount liste
                                                | null liste || length liste == 1 = 0
                                                | otherwise = if head liste == "the" &&  elem (head (head $ tail liste)) ['a', 'e', 'i', 'o', 'u']
                                                                then 1 + mycount (tail liste)
                                                                else mycount $ tail liste


--3-


    countVowels :: String -> Integer
    countVowels  = foldl foldFunction 0
                        where foldFunction acc ch = if ch `elem` ['a', 'e', 'i', 'o', 'u']
                                                        then acc + 1
                                                        else acc


--Validate the word


    newtype Word' = 
        Word' String 
        deriving (Eq, Show)

    vowels :: [Char]
    vowels = "aeiou"


    mkWord :: String -> Maybe Word'
    mkWord str = if countVowels str > fromIntegral (length $ filter (/=' ') str) - countVowels str
                    then Nothing
                    else Just $ Word' str


--It's only Natural


    data Nat = 
        Zero 
      | Succ Nat 
      deriving (Eq, Show)


    natToInteger :: Nat -> Integer
    natToInteger Zero = 0
    natToInteger (Succ nat) = 1 + natToInteger nat


    integerToNat :: Integer -> Maybe Nat
    integerToNat entier
        | entier < 0 = Nothing
        | otherwise = Just $ go entier
                    where go e
                                | e == 0 = Zero
                                | otherwise = Succ (go (e - 1))



--Small library for Maybe



--1-

    isJust :: Maybe a -> Bool
    isJust Nothing = False
    isJust (Just _) = True


    isNothing :: Maybe a -> Bool
    isNothing Nothing = True
    isNothing (Just _) = False


--2-

    mayybee :: b -> (a -> b) -> Maybe a -> b
    mayybee b _ Nothing = b
    mayybee _ f (Just v) = f v

--3-

    fromMaybe :: a -> Maybe a -> a
    fromMaybe deflt Nothing = deflt 
    fromMaybe _ (Just a) = a
   
--4-

    listToMaybe :: [a] -> Maybe a
    listToMaybe [] = Nothing
    listToMaybe (x:_) = Just x

--5-

    maybeToList :: Maybe a -> [a]
    maybeToList Nothing = []
    maybeToList (Just a)  = [a]


    
    catMaybes :: [Maybe a] -> [a]
    catMaybes = foldr step []
                        where step Nothing acc = acc
                              step (Just x) acc = x : acc

--6-

    flipMaybe :: [Maybe a] -> Maybe [a]
    flipMaybe = foldr step (Just [])
                    where step Nothing _ = Nothing
                          step (Just a) (Just acc) = Just $ a : acc
                          step (Just a) Nothing  = Nothing 


--Small library for Either 


--1-

    lefts' :: [Either a b] -> [a]
    lefts'  = foldr f []
                    where f (Left a) acc  = a:acc
                          f (Right _) acc  = acc


--2-

    rights' :: [Either a b] -> [b]
    rights' = foldr f []
                    where f (Left _) acc  = acc
                          f (Right b) acc  = b : acc


--3-

    partitionEithers' :: [Either a b] -> ([a], [b])
    partitionEithers' xs = (lefts' xs, rights' xs)


--4-

    eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
    eitherMaybe' fbc (Right b) = Just $ fbc b
    eitherMaybe' _ (Left _) = Nothing

--5-

    either' :: (a -> c) -> (b -> c) -> Either a b -> c
    either'  f _ (Left a)= f a
    either'  _ g (Right a)= g a

--6- 

{--eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c

eitherMaybe'' f (Left a) = Just $ either' _ f  (Left a)
eitherMaybe'' _ (Right _) = Nothing 

--}



--Unfolds


--Write your own iterate and unfoldr.


--1-

    myIterate :: (a -> a) -> a -> [a]
    myIterate f a  =  f a : myIterate f (f a)

--2-
    
    myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
    myUnfoldr f b0 =  go f (f b0)  b0
                    where go :: (b -> Maybe (a, b)) -> Maybe (a, b) -> b -> [a]
                            go _ Nothing _ = []
                            go func (Just (a, b)) b1 = a : go func (func b) b1

--3-

    betterIterate :: (a -> a) -> a -> [a]
    betterIterate f   = myUnfoldr (go f) 
                        where go :: (a -> a) -> (a -> Maybe(a, a))
                                (go ff) v = Just (v, ff v)


--Finally something other than a list

    data BinaryTree a = 
        Leaf 
        | Node (BinaryTree a) a (BinaryTree a)
        deriving (Eq, Ord, Show)

--1-

    unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
    unfold f v = go f (f v) v
                where go :: (a -> Maybe (a,b,a)) -> Maybe (a, b, a) -> a -> BinaryTree b
                    go ff Nothing  _ = Leaf
                    go ff (Just (a, b, c)) vv = Node (go ff  (ff a) vv) b (go ff  (ff c) vv)

--2-


    treeBuild :: Integer -> BinaryTree Integer
    treeBuild n = unfold f 0
        where f m
                | m == n = Nothing
                | otherwise = Just (m + 1, m , m + 1)

