{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-noncanonical-monoid-instances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse foldr/fmap" #-}
{-# HLINT ignore "Use traverse_" #-}
module Exams.ER2 where
import Distribution.Simple.Build (build)


-- Exo 1
-- Q1
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p l = let (a,b) = span p l in a : splitWith p (dropWhile (not . p) b)

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split x l = let (a,b) = span (/= x) l in a : split x (drop 1 b)

-- Q2


comp :: [Char] -> [Char] -> Int
comp w = length . filter (== w) . splitWith (== ' ')

-- Exo 2

class Cat cat where
    ident :: cat a a
    compose :: cat b c -> cat a b -> cat a c

-- kind de Cat : * -> * -> *  

instance Cat (->) where
    ident :: a -> a
    ident = id
    compose :: (b -> c) -> (a -> b) -> a -> c
    compose = (.)


(>>>) :: Cat cat => cat a b -> cat b c -> cat a c
f >>> g = compose g f

comp1 w = split ' '>>> filter (== w) >>> length

-- Q3
newtype ContextFun m a b = CF {runCF :: a -> m b}

readFileCF :: ContextFun IO FilePath String
readFileCF = CF readFile
printCF :: Show a => ContextFun IO a ()
printCF = CF print

-- printFileCF :: ContextFun IO FilePath ()
-- printFileCF = readFileCF >>> printCF

instance Monad m => Cat (ContextFun m) where
    ident :: Monad m => ContextFun m a a
    ident = ident
    compose :: Monad m => ContextFun m b c -> ContextFun m a b -> ContextFun m a c
    compose = compose

class Cat x => Arr x where
    arr :: (a -> b) -> x a b

instance Arr (->) where
    arr :: (a -> b) -> a -> b
    arr  = ident
    -- arr f a = f a

instance Monad m => Arr (ContextFun m) where
    arr :: Monad m => (a -> b) -> ContextFun m a b
    arr f = CF (return . f)

-- Exo 3

data Rose a = Node a [Rose a]
    deriving (Show, Eq,Ord)

leaf :: a -> Rose a
leaf x = Node x []

rose1 :: Rose Integer
rose1 = Node 15 [leaf 3, Node 45 [leaf 27, leaf 9, leaf 81]]

rose2 :: Rose (Integer, Integer)
rose2 = buildN 0 1

buildF :: Integer -> Integer -> [Rose (Integer, Integer)]
buildF lvl num = buildN lvl num : buildF lvl (num+1)

buildN :: Integer -> Integer -> Rose (Integer, Integer)
buildN lvl num = Node (lvl, num) (buildF (lvl+1) 1)

-- Q1
preorder :: Rose a -> [a]
preorder (Node x l) =   x : concatMap preorder l
-- ou 
-- preorder (Node x l) = x : concat [preorder r | r <- l]

-- >>> preorder rose1
-- [15,3,45,27,9,81]

-- Q2
-- parcours en largeur en partant de 0
level :: Integer -> Rose a -> [Rose a]
level 0 r = [r]
level n (Node _ l) = concatMap (level (n-1)) l


-- >>> level 0 rose1
-- [Node 15 [Node 3 [],Node 45 [Node 27 [],Node 9 [],Node 81 []]]]

-- Q3 

valuesAt :: Integer -> Rose a -> [a]
valuesAt lvl r = fmap (\(Node x _) -> x) (level lvl r)

-- on utilise un fmap sur un functor 

instance Functor Rose where
    fmap :: (a -> b) -> Rose a -> Rose b
    fmap f (Node x l) = Node (f x) (fmap (fmap f) l)
-- >>> fmap (*2) rose1
-- Node 30 [Node 6 [],Node 90 [Node 54 [],Node 18 [],Node 162 []]]


-- >>> valuesAt 2 rose1
-- [27,9,81]


-- >>> take 5 (valuesAt 0 rose2)
-- [(0,1)]

-- >>>  take 5 (valuesAt 1 rose2)
-- [(1,1),(1,2),(1,3),(1,4),(1,5)]

-- >>> take 5 (valuesAt 2 rose2)
-- [(2,1),(2,2),(2,3),(2,4),(2,5)]

-- >>> take 5 (valuesAt 3 rose2)
-- [(3,1),(3,2),(3,3),(3,4),(3,5)]


instance Foldable Rose where
    -- foldMap :: Monoid m => (a -> m) -> Rose a -> m
    foldMap f (Node x l) = f x <> foldMap (foldMap f) l -- doesn't work


-- Q5

-- roseFind :: (a -> Bool) -> Rose a -> Maybe a
-- roseFind f (Node x l) =
--     if f x then Just x
--     else foldr (<*>) Nothing (fmap (roseFind f) l) -- doesn't work


-- Q6

newtype First a = First {getFirst :: Maybe a}
    deriving (Show, Eq)

instance Semigroup (First a) where
    (<>) :: First a -> First a -> First a
    First Nothing <> x = x
    x <> _ = x

instance Monoid (First a) where
    mempty :: First a
    mempty = First Nothing

find :: Foldable t => (a -> Bool) -> t a -> Maybe a
find ok = getFirst . foldMap (\x -> First (if ok x then Just x else Nothing))

-- >>> find even rose1
-- Nothing

-- >>> find odd rose1
-- Just 15

-- >>> find (==3) rose1
-- Just 3


-- >>> find even [1,3,5,7,9] :: Maybe Integer
-- Nothing

-- >>> find odd [2,4,3,5,8,10] :: Maybe Integer
-- Just 3

-- >>> find (==3) [2, 5, 3, 8, 10] :: Maybe Integer
-- Just 3


-- Q7
instance Traversable Rose where
    -- traverse :: Applicative f => (a -> f b) -> Rose a -> f (Rose b)
    traverse :: Applicative f => (a -> f b) -> Rose a -> f (Rose b)
    traverse f (Node x l) = Node <$> f x <*> traverse (traverse f) l


printAll :: (Traversable t, Integral a, Show a) => a -> t a -> IO ()
printAll y tr = do
    traverse (printAux y) tr
    putStrLn ""
printAux :: (Integral p, Show p) => p -> p -> IO ()
printAux y x = do { aux ; putStr " " ; return () }
    where aux = putStr $ if x `mod` y == 0 then show x else "-"

-- >>> printAll 3 rose1
