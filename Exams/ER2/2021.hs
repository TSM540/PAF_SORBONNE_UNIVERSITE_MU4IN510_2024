-- {-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Exams.ER2 where
import Data.Semigroup 


-- Exo 1
data Triple a b c = Triple a b c
    deriving (Show, Eq)

-- Q1 
instance Functor (Triple a b) where
    fmap f (Triple x y z) = Triple x y (f z)

-- >>> fmap (+1) (Triple 1 2 3)
-- Triple 1 2 4

-- Q2

instance Applicative (Triple a b) where
    pure  = Triple undefined undefined 
    (Triple x y f) <*> (Triple x' y' z) = Triple x y (f z)


instance (Monoid a, Monoid b) => Monad (Triple a b) where
    (Triple x y z) >>= g = let (Triple x' y' z') = g z
                            in Triple (x <> x') (y <> y') z'

exTripleM :: Triple [Int] (Max Int) Int
exTripleM = do
            x <- Triple [1,2] (Max 12) 19
            y <- pure 13
            z <- Triple [3,4] (Max 15) 9
            return $ x + y + z + 1

-- >>> :t exTripleM 
-- Prelude.undefined

-- Exo 2


data RBColor = RED | BLACK
    deriving (Show, Eq, Ord)
data RBTree a =
            Node { color :: RBColor, val :: a, left :: RBTree a, right :: RBTree a}
            | Tip
            deriving (Show, Eq, Ord)
leaf :: RBColor -> a -> RBTree a
leaf col v = Node col v Tip Tip
exTree :: RBTree Int
exTree =
        Node BLACK 13
        (Node RED 8 (Node BLACK 1 Tip (leaf RED 6))
        (leaf BLACK 11))
        (Node RED 17 (leaf BLACK 15)
        (Node BLACK 25 (leaf RED 22)
        (leaf RED 27)))

-- Q1
search :: Ord a => a -> RBTree a -> Maybe (RBTree a)
search _ Tip = Nothing
search x (Node _ val l r)
    | x == val = Just (Node RED val l r)
    | x < val = search x l
    | otherwise = search x r

-- Q2

foldRB :: (RBColor -> a -> b -> b) -> b -> RBTree a -> b
foldRB _ acc Tip = acc
foldRB f acc (Node col val l r) = f col val (foldRB f (foldRB f acc r) l)
data Pnat = S Pnat | Z

-- Q3

instance Foldable RBTree where
    foldMap f = foldRB (\_ x acc -> f x <> acc) mempty

-- >>> foldMap Sum exTree
-- Sum {getSum = 145}
-- ! works

-- Q4


prop_bst :: Ord a => RBTree a -> Bool
prop_bst = undefined

-- Q5

-- Proposer une implémentation de la propriété suivante :
-- prop_red :: RBTree a -> Bool
-- Les racines des sous-arbres gauche et droit d’un nœud interne rouge sont de couleur noire. Et les deux
-- sous-arbres eux-mêmes respectent la propriété.
-- Remarque : les terminaux sont considérés de couleur noire

-- >>> prop_red exTree
-- No match in record selector color
-- 
prop_red :: RBTree a -> Bool
-- Les racines des sous-arbres gauche et droit d’un nœud interne rouge sont de couleur noire. Et les deux
prop_red (Node RED _ l r) = color l == BLACK && color r == BLACK && prop_red l && prop_red r
prop_red (Node _ _ l r) = prop_red l && prop_red r
prop_red Tip = True






-- idk the rest

