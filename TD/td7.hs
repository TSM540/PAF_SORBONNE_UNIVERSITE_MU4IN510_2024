
newtype Lecteur e a = Lecteur { runLecteur :: e -> a }


instance Functor (Lecteur e ) where 
    -- fmap :: (a -> b) -> Lecteur e a -> Lecteur e b
    fmap g (Lecteur f) = Lecteur (g . f)

instance Applicative (Lecteur e) where
    -- pure :: a -> Lecteur e a
    pure x = Lecteur (\_ -> x)
    -- (<*>) :: Lecteur e (a -> b) -> Lecteur e a -> Lecteur e b
    (Lecteur f) <*> (Lecteur g) = Lecteur (\x -> f x (g x))
--  <*> (Lecteur f)  (Lecteur g) = Lecteur (\x -> f x (g x))

r1 :: Lecteur Int Int
r1 = (+1) <$> Lecteur (\x -> x - 1)
-- >>>:t r1
-- r1 :: Lecteur Int Int
r2 :: Lecteur Int Int
r2 = (+) <$> Lecteur (+3) <*> Lecteur (*100)
--   <Lecteur Int (Int ->Int)>  | <Lecteur Int Int>    
-- >>>:t r2
-- r2 :: Lecteur Int Int
-- >>> (runLecteur r2) 5
-- 508

-- >>> (runLecteur r2) 10
-- 1013


data Paire a b = Paire a b
    deriving(Show, Eq)

instance Functor (Paire e) where
    fmap g (Paire x y) = Paire x (g y)

instance (Monoid e)=> Applicative (Paire e) where
    -- pure x = Paire mempty x
    pure = Paire mempty
    (<*>) (Paire e1 f)  (Paire e2 x) = Paire (e1 <> e2) (f x)

-- >>> (+1) <$> (Paire "abc" 3)
-- Paire "abc" 4

-- >>> (+) <$> (Paire "Hello" 3) <*> (Paire " World!" 5)
-- Paire "Hello World!" 8


-- >>> (+) <$> [1,2,3] <*> [10,20,30]
-- [11,21,31,12,22,32,13,23,33]

-- un peut comme :  for i in [1,2,3] for j in [10,20,30] do t1[i]+t2[j]

-- >>> [(+),(*)] <*> [1,2,3] <*> [10,20,30]
-- [11,21,31,12,22,32,13,23,33,10,20,30,20,40,60,30,60,90]

-- les ziplistes

newtype ZList a = ZL [a] deriving (Show, Eq)

instance Functor ZList where
    fmap f (ZL l) = ZL (f <$> l)

-- >>> (+1) <$> (ZL [1,2,3])
-- ZL [2,3,4]

-- >>> applyZL ((+) <$> (ZL [1,2,3])) (ZL [10,11])
-- ZL [11,13]

-- >>> applyZL ((+) <$> (ZL [1,2,3])) (ZL [10,11,12,14,120])
-- ZL [11,13,15]

-- >>> applyZL ((+) <$> (ZL [1,2,3])) (ZL [])
-- ZL []

applyZL :: ZList (a -> b) -> ZList a -> ZList b
applyZL(ZL []) _ = ZL []
applyZL _ (ZL [])  = ZL []
applyZL (ZL (f:fs)) (ZL (x:xs)) = let ZL l = applyZL (ZL fs) (ZL xs) in ZL (f x:l)

-- pureZL :: a -> ZList a
-- pureZL x = ZL [x]

-- >>> (pureZL id) `applyZL` (ZL [1,2,3])
-- ZL [1]

-- >>> id <$> (ZL [1,2,3])
-- ZL [1,2,3]


pureZL :: a -> ZList a
pureZL  = ZL . repeat
instance Applicative ZList where
    pure = pureZL
    (<*>) = applyZL

-- >>> (ZL [(+),(*)]) <*> ZL [1,2,3] <*> ZL [10,20,30]
-- ZL [11,40]


-- ! Exo 3

data Tree a =
            Tip
            | Node a (Tree a) (Tree a)
            deriving (Show, Eq)
exTree1 :: Tree Integer
exTree1 = Node 17 (Node 24 (Node 12 (Node 9 Tip Tip) Tip)
            (Node 42 Tip Tip))
            (Node 19 Tip (Node 11 Tip Tip))
exTree2 :: Tree Integer
exTree2 = Node 18 (Node 24 (Node 12 (Node 8 Tip Tip) Tip)
            (Node 42 Tip Tip))
            (Node 20 Tip (Node 12 Tip Tip))
treeFoldMap :: Monoid m => (a -> m) -> Tree a -> m
treeFoldMap _ Tip = mempty
treeFoldMap f (Node v l r) = f v <> treeFoldMap f l <> treeFoldMap f r

arbreFoldMap :: Monoid m => (a -> m) -> Tree a -> m
arbreFoldMap _ Tip = mempty
arbreFoldMap f (Node v l r) = arbreFoldMap f l <> f v  <> arbreFoldMap f r

instance Foldable Tree where
    -- foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap = arbreFoldMap

instance Functor Tree where
    fmap _ Tip = Tip
    fmap f (Node v l r) = Node (f v) (fmap f l) (fmap f r)
-- >>> (+1) <$> exTree1
-- Node 18 (Node 25 (Node 13 (Node 10 Tip Tip) Tip) (Node 43 Tip Tip)) (Node 20 Tip (Node 12 Tip Tip))

traverseArbre :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
traverseArbre _ Tip = pure Tip
traverseArbre f (Node v l r) = Node <$> f v <*> traverseArbre f l <*> traverseArbre f r


instance Traversable Tree where 
    -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse = traverseArbre
paireArbre :: Integral a => Tree a -> Maybe (Tree a)
paireArbre = traverse (\x -> if even x then Just x else Nothing)

-- >>> paireArbre exTree1
-- Nothing

-- >>> paireArbre exTree2
-- Just (Node 18 (Node 24 (Node 12 (Node 8 Tip Tip) Tip) (Node 42 Tip Tip)) (Node 20 Tip (Node 12 Tip Tip)))


-- ! Exo 4

type Erreur = String 

verifPair :: (Integral a, Show a) => a -> Either [Erreur] a
verifPair n | even n = Right n
            | otherwise = Left ["Le nombre " ++ show n ++ " n'est pas pair"]
-- >>> verifPair 4
-- Right 4

-- >>> verifPair 3
-- Left ["Le nombre 3 n'est pas pair"]

newtype Validation e a = Val (Either e a ) deriving (Show, Eq)

instance Functor (Validation e) where 
    fmap f (Val ei) = Val (fmap f ei)

-- >>> (+1) <$> (Val (Right 3))
-- Val (Right 4)

instance Monoid e => Applicative (Validation e) where
    pure = Val . Right
    (<*>) = applyVal

applyVal :: (Monoid e) => Validation e (a -> b) -> Validation e a -> Validation e b
-- er == liste d'erreur 
applyVal (Val (Left er)) (Val (Right x)) = Val (Left er)
applyVal (Val (Right x)) (Val (Left er))  = Val (Left er)
applyVal (Val (Right f)) (Val (Right x))  = Val (Right (f x))
applyVal (Val (Left er1)) (Val (Left er2))  = Val (Left (er1 <> er2))


type Valid a = Validation [Erreur] a
valide :: a -> Valid a
valide = pure

invalide :: String -> Valid a
invalide = Val . Left . (:[])

validePair :: (Integral a, Show a) => a -> Valid a
validePair n | even n = pure n
               | otherwise = invalide ("Le nombre " ++ show n ++ " n'est pas pair")

valideNonVide ::String -> Valid()
valideNonVide "" = invalide "La chaine est vide"
valideNonVide xs = valide ()

valideMiniscule :: Char -> Valid Char
valideMiniscule c | c `elem` ['a'..'z'] = valide c
                  | otherwise = invalide ("Le caractère " ++ show c ++ " n'est pas minuscule")

valideMajuscule :: Char -> Valid Char
valideMajuscule c | c `elem` ['A'..'Z'] = valide c
                  | otherwise = invalide ("Le caractère " ++ show c ++ " n'est pas majuscule")
valideNom :: String -> Valid String
valideNom str = (:) <$> (vNE *> vH) <*> vT 
    where 
        vNE = valideNonVide str
        vH = case str of 
            [] -> Val $ Left ["Ne deverait pas arriver"]
            (c:_) -> valideMajuscule c
        vT = case str of 
            [] -> Val $ Left ["Ne deverait pas arriver"]
            (_:cs) -> traverse  valideMiniscule cs