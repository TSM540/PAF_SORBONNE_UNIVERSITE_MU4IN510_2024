-- ! Les Foncteurs
-- import Prelude hiding (Functor, fmap, Monoid, mempty, mappend, Semigroup, (<>))
-- * Les foncteurs

-- ** Définition

-- Les foncteurs sont des types de données qui peuvent être mappés dans un contexte.

-- ** Exemple

-- Les listes sont des foncteurs.

data MyData a = MyData a deriving Show

instance Functor MyData where
  fmap :: (a -> b) -> MyData a -> MyData b
  fmap f (MyData a) = MyData (f a)

-- c'est comme si applique une fonction à une valeur dans un contexte, 
-- et le contexte(boite) c'est notre data.
-- ** Exemple

-- Les arbres sont des foncteurs.
data Tree a = Leaf  | Node a (Tree a) (Tree a) deriving Show
instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node  v l r) =  Node (f v) (fmap f l) (fmap f r)

-- ! les lois de foncteur
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (f . g ) x=  f (g x)
-- 1. fmap id = id
-- 2. fmap (f . g) = fmap f . fmap g
-- 3. fmap (f . g . h) x = fmap f. (fmap g . (fmap h x))

-- ! Les Monoïdes

-- ** Définition

-- Un monoïde est un ensemble muni d'une loi de composition interne associative et d'un élément neutre pour cette composition.

-- ** Exemple

data MonoidData  = MonoidData a deriving Show
instance Monoid  (MonoidData a ) where
    mempty = MonoidData a
    -- mappend (MonoidData a) (MonoidData b) = MonoidData (a <> b)
    -- ou bien
    (<>) :: MonoidData a -> MonoidData a -> MonoidData a
    (MonoidData a) <> (MonoidData b) = MonoidData (a <> b)

-- : lois de monoid
-- x <> mempty = x
-- mempty <> x = x
-- x <> (y<>z) = (x<>y)<>z

-- ** Exemple
-- Parcours G Racine D
treeFoldMap :: Monoid m => (a -> m) -> Tree a -> m
treeFoldMap f (Leaf a) = mempty 
treeFoldMap f (Node v l r) = treeFoldMap f l <> f v <> treeFoldMap f r

-- ! Semi Groupe 
-- on peut utiliser la fonction (<>) pour concatener deux listes par exemples, il existe cependant d'autres utilisations.
class Semigroup a where
    (<>) :: a -> a -> a
instance Semigroup [a] where
    (<>) = (++) -- conactenation de deux listes


-- ! Applictive 

-- ** Définition

-- Les applicatives sont des foncteurs qui permettent d'appliquer des fonctions contenues dans un
-- contexte à des valeurs contenues dans un contexte.
-- Traduction : c'est comme si on avait une fonction dans une boite et une valeur dans une autre boite plusieurs fois

-- ** Exemple
data MyApplicativeData a = MyApplicativeData a deriving Show

instance Prelude.Functor MyApplicativeData where
  fmap :: (a -> b) -> MyApplicativeData a -> MyApplicativeData b
  fmap f (MyApplicativeData x) = MyApplicativeData (f x)

instance Applicative MyApplicativeData where
    pure :: a -> MyApplicativeData a
    pure x = MyApplicativeData x
    -- Apply
    (<*>) :: MyApplicativeData (a -> b) -> MyApplicativeData a -> MyApplicativeData b
    (MyApplicativeData f) <*> (MyApplicativeData x) = MyApplicativeData (f x)
-- ça c'esy la base, pour mieux voir ça, on peut utiliser notre fonction sur plusieurs contexte 

-- ma_fonction <$> arg1_dans_contexte <*> arg2_dans_contexte  <*> arg3_dans_contexte  <*> arg3_dans_contexte
-- <$> c'est un fmap
newtype Read a b = Read { runRead :: a -> b }
instance Functor (Read a) where
  fmap f (Read g) = Read (f . g)

instance Applicative (Read a) where
    pure x = Read (\_ -> x)
    (Read f) <*> (Read g) = Read (\x -> f x (g x))
r1 :: Read Int Int
r1 = (+) <$> Read (+2) <*> Read (*3)
-- >>> runRead r1 2
-- 10 
-- car on fait (2 (+2)) + (2 (*3) ) = 10

-- : Lois de Applicative

-- 1. pure id <*> v = v
-- 2. pure f <*> pure x = pure (f x)
-- 3. f <*> pure x = pure ($ x) <*> f
 -- ou bien 
     -- f <*> pure x = pure (\g -> g x) <*> f
-- 4. f <$> x = pure f <*> x

-- ! Monad

(>>=) :: (Monad m) => (a -> m b) -> m a -> m b
(>>=) f x = x >>= f

-- ** Définition

-- Les monades sont des foncteurs applicatives qui permettent de chaîner des fonctions 
-- qui retournent des valeurs contenues dans un contexte.
-- la monade utilise le retour de la fonction précédente pour l'appliquer à fonction suivante
-- pour résumer : f >>= g >>= h
-- c'est comme si on avait une fonction f, puis le résultat sera passé à g, puis le résultat sera passé à h
-- ** Exemple

data MyMonadData a = MyMonadData a deriving Show

instance Functor MyMonadData where
  fmap :: (a -> b) -> MyMonadData a -> MyMonadData b
  fmap f (MyMonadData x) = MyMonadData (f x)

instance Applicative MyMonadData where
    pure :: a -> MyMonadData a
    pure x = MyMonadData x
    -- Apply
    (<*>) :: MyMonadData (a -> b) -> MyMonadData a -> MyMonadData b
    (MyMonadData f) <*> (MyMonadData x) = MyMonadData (f x)

instance Monad MyMonadData where
    (>>=) :: MyMonadData a -> (a -> MyMonadData b) -> MyMonadData b
    (MyMonadData x) >>= f = f x

-- ! Les lois de monade
-- 1. return a >>= f = f a
-- 2. f >>= return = f
-- 3. (f >>= g) >>= h = f >>= (g >>= h)

-- il existe cependant plusieurs types de monade, a adapter selon le contexte

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(>=>) f g x = f x >>= g
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
(<=<) f g x = g x >>= f



-- * exemples

data TreeMonad a = Tip a | Noeud (TreeMonad a) (TreeMonad a)
  deriving (Show, Eq)

instance Functor TreeMonad where
    fmap f (Tip x) = Tip (f x)
    fmap f (Noeud l r) = Noeud (fmap f l) (fmap f r)

instance Applicative TreeMonad where
    pure x = Tip x
    Tip f <*> Tip x = Tip (f x)
    Noeud l r <*> t = Noeud (l <*> t) (r <*> t)

instance Monad TreeMonad where
    Tip x >>= f = f x
    Noeud l r >>= f = Noeud (l >>= f) (r >>= f)

-- ! Les lois de monade pour notre TreeMonad
prop_IdDroite_Loi :: (Monad m, Eq (m b)) => (a -> m b) -> a -> Bool
prop_IdDroite_Loi f e = (f e >>= pure) == f e

prop_IdGauche_Loi :: (Monad m, Eq (m b)) => (a -> m b) -> a -> Bool
prop_IdGauche_Loi f e = (pure e >>=  f) == f e

prop_Associativite_Loi :: (Monad m, Eq (m d)) => (a -> m b) -> (b -> m c) -> (c -> m d) -> a -> Bool
prop_Associativite_Loi f g h e = ((f >=> g) >=> h) e == (f >=> (g >=> h)) e

