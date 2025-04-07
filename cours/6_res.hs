-- ! Composition
-- id :: a -> a ( retourne elle même)

-- ## Composition
-- >>> :t (.)
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- ### -- Implémentation possible :
-- (.) g f x = g (f x)
law_Cat_rightId :: Eq b => (a -> b) -> a -> Bool
law_Cat_rightId f x = (f . id) x == f x
-- aka, une fonction f composee avec id et qui prend comme argument x doit retourner f x

-- ! functor 
-- :Autres exemples, si on execute Just 3, on obtient Just 3
-- ! mais si on execute (+1) (Just 3), on obtient une erreur
-- et dans ce cas on peut utiliser les Functor
data Maybe2 = Nothing2 | Just2 Int
  deriving (Show, Eq)
instance Functor Maybe2 where
  fmap _ Nothing2 = Nothing2
  fmap g (Just2 x) = Just2 (g x)

-- another great example is with the trees 
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Eq)

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

-- définir nos functor, verifier nos lois

-- ! Semi Groupe 
-- on peut utiliser la fonction (<>) pour concatener deux listes par exemples, il existe cependant d'autres utilisations.
class Semigroup a where
    (<>) :: a -> a -> a
instance Semigroup [a] where
    (<>) = (++)
law_Semigroup_assoc :: (Eq a, Semigroup a) => a -> a -> a -> Bool
law_Semigroup_assoc x y z = x <> (y <> z) == (x <> y) <> z

-- ! Monoid
type Monoid :: * -> Constraint
class (Semigroup a) => Monoid a where
    mempty :: a
    mappend :: a -> a -> a
    mconcat :: [a] -> a
    {-# MINIMAL mempty #-}
law_Monoid_mempty_left :: (Monoid a, Eq a) => a -> Bool
law_Monoid_mempty_left x = mempty <> x == x

law_Monoid_mempty_right :: (Monoid a, Eq a) => a -> Bool
law_Monoid_mempty_right x = x <> mempty == x

-- >>> :info List
-- type List :: * -> *
-- data List a = Nil | Cons a (List a)
concatList :: List a -> List a -> List a
concatList Nil ys = ys
concatList xs Nil = xs
concatList (Cons x xs) ys = Cons x $ concatList xs ys
-- : instance
instance Semigroup (List a) where
  (<>) = concatList
instance Monoid (List a) where
  mempty = Nil

type Product :: * -> *
newtype Product a = Product { getProduct :: a }
  deriving (Show, Eq)

-- >>> (Product 3) <> (Product 4)
