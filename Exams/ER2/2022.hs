{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE UndecidableInstances #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Exams.ER2 where

-- Exercise 1

newtype State s a  = State { runState :: s -> (a, s) }

get :: State s s
get  = State $ \q -> (q, q)

put :: s -> State s ()
put s = State $ \_ -> ((), s)


instance Applicative (State s) => Monad (State s ) where
    return :: a -> State s a
    return x = State $ \q -> (x, q)

    (>>=) :: State s a -> (a -> State s b) -> State s b
    (State step ) >>= g =
            State $  \q -> let (x, q') = step q
                              in runState (g x) q'


class IMonad m where
    ireturn :: a -> m s s a
    ibind :: m s t a -> (a -> m t u b) -> m s u b
-- On déﬁnit également des opérateurs bind inﬁxes :
    (>>>=) :: IMonad m => m s t a -> (a -> m t u b) -> m s u b
    (>>>=) = ibind

infixl 1 >>>=
(>>>) :: IMonad m => m s t a -> m t u b -> m s u b
st1 >>> st2 = st1 >>>= \_ -> st2
infixl 1 >>>

newtype IState s t a = IState { runIState :: s -> (a, t) }

instance IMonad IState where
    ireturn x = IState $ \q -> (x, q)
    ibind (IState step) g = IState $ \q -> let (x, q') = step q
                                           in runIState (g x) q'

iget:: IState s s s
iget = IState $ \q -> (q, q)

iput :: t -> IState s t ()
iput s = IState $ \_ -> ((), s)

evalIState :: IState s t a -> s -> a
evalIState st q = fst (runIState st q)

data Started = Started
data Stopped = Stopped

start :: IState i Started ()
start = iput Started

stop :: IState Started Stopped ()
stop = iput Stopped

use :: (a -> a) -> a -> IState Started Started a
use g x = IState (\_ -> (g x, Started))
type SafeUse a = IState a Stopped a


-- Q 3) jsp 

-- Ex 02

type NodeId = Int
data Node a = Node {
                preds ::[NodeId]
                , nid :: NodeId
                , val :: a
                , succs :: [NodeId]
              }
                 deriving (Show, Eq)
data Graph a = GEmpty | GBuild (Node a) (Graph a)
    deriving Show


-- invariant construction d'un graph 
-- 1) les noeuds sont uniques
-- 2) les prédécesseurs et les successeurs sont uniques


-- Q2
invariantConstruction :: Graph a -> Bool
invariantConstruction GEmpty = True
invariantConstruction (GBuild n g) = invariantConstruction g && invariantNode n g && invariantSuccs n g && invariantPreds n g

invariantNode :: Node a -> Graph a -> Bool
invariantNode n GEmpty = True
invariantNode n (GBuild n' g) = nid n /= nid n' && invariantNode n g

invariantSuccs :: Node a -> Graph a -> Bool
invariantSuccs n GEmpty = True
invariantSuccs n (GBuild n' g) = notElem (nid n) (succs n') && invariantSuccs n g

invariantPreds :: Node a -> Graph a -> Bool
invariantPreds n GEmpty = True
invariantPreds n (GBuild n' g) = notElem (nid n) (preds n') && invariantPreds n g
-- Q2
gmap :: (Node a -> Node b) -> Graph a -> Graph b
gmap _ GEmpty = GEmpty
gmap f (GBuild n g) = GBuild (f n) (gmap f g)

instance Functor Graph where
    fmap f = gmap (\n -> n {val = f (val n)})

-- Q3 

greverse :: Graph a -> Graph a
greverse = gmap (\n -> n {preds = succs n, succs = preds n})

-- Q4
-- gmap id = id
--  car 
-- gmap id GEmpty = GEmpty 
-- gmap id (GBuild n g) = GBuild (id n) (gmap id g) = GBuild n (gmap id g) = GBuild n g




-- gmap f . gmap g  =gmap (f . g)
-- car 
-- gmap f . gmap g GEmpty = gmap f (gmap g GEmpty) = gmap f GEmpty = GEmpty
-- gmap f . gmap g (GBuild n g') = gmap f (gmap g (GBuild n g')) = gmap f (GBuild (g n) (gmap g g')) = GBuild (f (g n)) (gmap f (gmap g g')) = GBuild (f (g n)) (gmap (f . g) g') = GBuild (f . g $ n) (gmap (f . g) g') = gmap (f . g) (GBuild n g')

-- greverse . greverse = id car
-- greverse . greverse GEmpty = greverse GEmpty = GEmpty
-- greverse . greverse (GBuild n g) = greverse (greverse (GBuild n g)) = greverse (GBuild n' (greverse g)) = GBuild n' (greverse (greverse g)) = GBuild n' g = GBuild n g




-- Q5
gfold :: (Node a -> b -> b) -> b -> Graph a -> b
gfold _ acc GEmpty = acc
gfold f acc (GBuild n g) = f n (gfold f acc g)

-- implémenter la fonction nodes :: Graph a -> [a] qui retourne une liste des nœuds d’un graphe g
nodes :: Graph a -> [a]
nodes = gfold (\n acc -> val n : acc) []

-- réimplémenter la fonction gmap

gmap' :: (Node a -> Node b) -> Graph a -> Graph b
gmap' f = gfold (GBuild . f) GEmpty

-- Q6

-- Déﬁnir la fonction :
-- successors :: Graph a -> NodeId -> [NodeId]
-- qui retourne dans un graphe g une liste des successeurs directs d’un nœud identiﬁé par son identiﬁant nid


successors :: Graph a -> NodeId -> [NodeId]
successors GEmpty _ = []
successors (GBuild node@(Node p n v s) g) nid = if nid == n then succs node else successors g nid