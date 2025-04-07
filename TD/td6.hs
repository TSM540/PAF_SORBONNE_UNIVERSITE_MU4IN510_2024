law_Functor_id :: (Functor f, Eq (f a)) => f a -> Bool
law_Functor_id x = (fmap id x) == id x
law_Functor_comp :: (Functor f, Eq (f c)) => (b -> c) -> (a -> b) -> f a -> Bool
law_Functor_comp g f x = fmap (g . f) x == (fmap g . fmap f) x

id :: a -> a
id x = x

infixr 9 .
(.) :: (b -> c) -> (a -> b) -> a -> c
g . f x = g (f x)

-- (f . g) on applique g puis f


-- explication de fmap
typeclass Functor f where
fmap :: (a -> b) -> f a -> f b
fmap ::(a -> b) -> Maybe a -> Maybe b
fmap _ Nothing = Nothing
fmap f (Just x) = Just (f x)

-- Q1.1


-- data List a = Nil | Cons a (List a) deriving (Show, Eq, Ord)
-- listMap :: (a -> b) -> List a -> List b
-- listMap _ Nil = Nil
-- listMap f (Cons x xs) = Cons (f x) (listMap f xs)
 
-- instance Functor List where
-- fmap = listMap
-- ! preuve f.g
-- (.). id = id
-- ((.) .id) f g // (id f) g x
-- ((.) f) g  // f ( g x)
-- => f .g
-- ! preuve f.g.h
-- (.)( f.g) h = ((.) f) ((.) g) h
-- = (.) f (g h)
--
-- Q1.2
data Pair a b = Pair a b
    deriving (Show, Eq)

-- fmap (+1) Pair 1 2
-- ce n'est pas possible car Pair n'est pas une instance de Functor
data Pair2 a = Pair2 a a
    deriving (Show, Eq)
-- Pair : * -> * -> *
-- Pair a : * -> *
instance Functor (Pair a) where 
    fmap :: (u -> v) -> Pair a u -> Pair a v
    fmap f (Pair x y) = Pair x (f y)
    --   u->v                a   v
instance Functor Pair2 where 
    fmap :: (u->v) -> Pair2 u -> Pair2 v
    fmap f (Pair2 x y ) = Pair(f x) (f y)
 --     u->v      u u          v     v

-- Q1.3
-- (->) :: * -> * -> *
-- (a ->) :: * -> *
flecheMap :: (u->v) -> (a -> u) -> (a -> v)
flecheMap = (.)

-- les lois de monoid
-- x <> mempty = x
-- mempty <> x = x
-- x <> (y<>z) = (x<>y)<>z
newtype Reader e a = Reader ( e -> a )
instance Monoid (Reader e a) where 
        mempty = Reader e a 
        mempty = Reader(\x -> mempty)
        --      Monoid a                    e ->a
        (<>) :: Reader e a > Reader e a -> Reader e a
        (Reader f) <>(Reader g) = 
                Reader (\x -> (f x) <> (g x))
                --       e      a        a
                --                   a

-- $ Exercice 3
data Tree a =
    Tip
    | Node a (Tree a) (Tree a)
    deriving (Show, Eq)
exTree :: Tree Integer
exTree = Node 17 (Node 24 (Node 12 (Node 9 Tip Tip) Tip)
(Node 42 Tip Tip))
(Node 19 Tip (Node 11 Tip Tip))

-- Q3.1
-- parcours G Racine  D

treeFoldMap :: Monoid m => (a -> m) -> Tree a -> m
treeFoldMap f Tip = mempty
treeFoldMap f (Node x l r) =  treeFoldMap f l <> f x  <> treeFoldMap f r
-- parcours Racine G D
treeFoldMap2 :: Monoid m => (a -> m) -> Tree a -> m
treeFoldMap2 f Tip = mempty
treeFoldMap2 f (Node x l r) = f x <> treeFoldMap2 f l <> treeFoldMap2 f r
affiche :: (Show a ) => Tree a -> String
affiche  = treeFoldMap show
-- Q 3.2
class Foldable t where
    foldMap :: Monoid m => (a -> m) -> t a -> m  -- general 
    foldMap :: Monoid m => (a -> m) -> Tree a -> m --particulier
    foldMap T = treeFoldMap T

ListeEtq :: Tree a -> [a]
ListeEtq = treeFoldMap (\x -> [x])

foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

myFoldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
myFoldMap  f   p = foldr step mempty p
    --   a->m  t a 
    where
        step x acc = f x <> acc

-- newtype Mono = Vide | Mo a deriving (Show, Eq)
newtype Lecteur e a = L(e -> a) deriving (Show, Eq)
instance Semigroupe (Lecteur e) where 
    L f <> L g = L $ f g
instance Monoid (Lecteur e) where 
    mempty = Lecteur id 
myFoldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
myFoldr f       i   p = 
    -- a->b->b  b  t a
     let Interm y = foldMap (\x -> Interm (f x)) p
                       --     a          a->b->b ta
     in  y    i 
    --  b->b  b