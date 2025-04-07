
import qualified Data.Map.Strict as M
import Control.Monad 

pokedex :: M.Map Int String 
pokedex = M.fromList [(1, "Flammignon"), 
                    (2, "Aquasaucisse"), 
                    (3, "Plantenpot"),
                    (4,"Sacavin"),
                    (18,"Hippopobraise"),
                    (42,"Océanguez"),
                    (124,"Mammouthenfer")
                    ]

evolutions :: M.Map Int Int
evolutions = M.fromList [(1, 18), 
                        (2, 42), 
                        (18, 124)
                        ]

trouvePoke :: Int -> Maybe String
trouvePoke = flip M.lookup pokedex

trouveEvo :: Int -> Maybe Int 
trouveEvo = flip M.lookup evolutions
-- >>= :: (a -> m b ) -> m a -> m b
-- >>= :: (a ->boite(b)) -> boite(a) -> boite(b)
-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
-- (1) pure >=> f = f (identité gauche)
-- (2) f >=> pure = f (identité droite)
-- • pour tout a, b, c, d et f:: a -> m b, g:: b -> m c et h:: c -> m d on a :
-- (3) (f >=> g) >=> h = f >=> (g >=> h) (associativité)


-- >>> trouvePoke 3
-- Just "Plantenpot"

-- >>> trouvePoke 5 
-- Nothing

-- >>> trouveEvo  >=> trouvePoke $ 1
-- Just "Hippopobraise"
-- >>> trouveEvo  >=> trouvePoke $ 42
-- Nothing

-- >>> (trouveEvo 3) >>= trouveEvo >>=  trouvePoke 
-- Nothing
-- >>> (trouveEvo 2) >>= trouveEvo >>=  trouvePoke 
-- Nothing

-- >>>

-- >>> (trouveEvo 1) >>= trouveEvo >>=  trouvePoke
-- Just "Mammouthenfer"

trouvePokevoevo :: Int -> Maybe String 
trouvePokevoevo n = case trouveEvo n of 
                    Nothing -> Nothing
                    Just n2 -> case trouveEvo n2 of 
                                Nothing -> Nothing
                                Just n3 -> trouvePoke n3


-- >>> fmap trouvePokevoevo [1,2,18,124,3,5]                                
-- [Just "Mammouthenfer",Nothing,Nothing,Nothing,Nothing,Nothing]

trouvePokevoevo2 :: Int -> Maybe String
trouvePokevoevo2 n = trouveEvo n >>= trouveEvo >>= trouvePoke
-- >>> fmap trouvePokevoevo2 [1,2,18,124,3,5]                                
-- [Just "Mammouthenfer",Nothing,Nothing,Nothing,Nothing,Nothing]

-- >=> :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
-- >=> :: (a -> boite (b)= -> (b -> boite(c)) -> a -> boite(c)
-- <=< :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
-- <=< :: (b -> boite(c)) -> (a -> boite(b)) -> a -> boite(c)
  
trouvePokevoevo3 :: Int -> Maybe String
trouvePokevoevo3  = trouveEvo >=> trouveEvo >=> trouvePoke
-- >>> fmap trouvePokevoevo3 [1,2,18,124,3,5]     
-- [Just "Mammouthenfer",Nothing,Nothing,Nothing,Nothing,Nothing]

prop_idGauche_loi :: (Monad m,Eq (m b)) => (a -> m b) -> a -> Bool
prop_idGauche_loi f e = (pure >=> f) e == f e

-- >>> prop_idGauche_loi trouvePoke 1
-- True
-- >>> fmap (prop_idGauche_loi trouvePoke ) [1,2,3,4,5,42,43]
-- [True,True,True,True,True,True,True]

prop_idDroite_loi :: (Monad m,Eq (m b)) => (a -> m b) -> a -> Bool
prop_idDroite_loi f e = (f >=> pure) e == f e
-- >>> fmap (prop_idDroite_loi trouvePoke ) [1,2,3,4,5,42,43]
-- [True,True,True,True,True,True,True]

prop_associativite_loi :: (Monad m,Eq (m d)) => (a -> m b) -> (b -> m c) -> (c -> m d) -> a -> Bool
prop_associativite_loi f g h e = ((f >=> g) >=> h) e == (f >=> (g >=> h)) e
-- >>> prop_associativite_loi trouveEvo trouveEvo trouvePoke 1
-- True
-- >>> fmap (prop_associativite_loi trouveEvo trouveEvo trouvePoke ) [1,2,3,4,5,42,43]
-- [True,True,True,True,True,True,True]

poissonD ::(Monad m) => (a -> m b) -> (b -> m c) -> (a -> m c)
poissonD f g e= f e >>= g

prop_poissonD :: (Monad m, Eq (m c)) => (a -> m b) -> (b -> m c) -> a -> Bool
prop_poissonD f g e = (f >=> g) e == poissonD f g e
-- >>> fmap (prop_poissonD trouveEvo trouvePoke ) [1,2,3,4,5,42,43]
-- [True,True,True,True,True,True,True]


--- ! on écrit les lois en utilisant bind

prop_idGauche2_loi :: (Monad m,Eq (m b)) => (a -> m b) -> a -> Bool
prop_idGauche2_loi f e = (pure e >>= f) == f e
-- >>> fmap (prop_idGauche2_loi trouvePoke ) [1,2,3,4,5,42,43]
-- [True,True,True,True,True,True,True]

prop_idDroite2_loi :: (Monad m,Eq (m b)) => (a -> m b) -> a -> Bool
prop_idDroite2_loi f e = (f e >>= pure) == f e

-- >>> fmap (prop_idDroite2_loi trouvePoke ) [1,2,3,4,5,42,43]
-- [True,True,True,True,True,True,True]

prop_associativite2_loi :: (Monad m,Eq (m d)) => (a -> m b) -> (b -> m c) -> (c -> m d) -> a -> Bool
prop_associativite2_loi f g h e = (((\z -> f z >>= g) e) >>= h) == (f e >>= (\z -> g z >>= h))
-- >>> fmap (prop_associativite2_loi trouveEvo trouveEvo trouvePoke ) [1,2,3,4,5,42,43]

newtype Identite a = Id { ouvreId :: a }
            deriving (Show, Eq)
instance Functor Identite where
            fmap g (Id x) = Id (g x)
instance Applicative Identite where
            pure = Id
            (Id g) <*> (Id x) = Id (g x)
instance Monad Identite where
    (Id x) >>= f = f x
-- (Inversion) v == Id (OuvreId v)

--- *** Monades ****

type Pile a =[a]
pop :: Pile a -> (a,Pile a)
pop [] = error "Pile vide"
pop (x:xs) = (x,xs)

push :: a -> Pile a -> ((),Pile a)
push x xs = ((),x:xs)

-- >>> pop [1,2,3,4]
-- (1,[2,3,4])

-- >>> push 5 [1,2,3,4]
-- ((),[5,1,2,3,4])

-- >>> (push 5 . pop 5 ) []
-- Couldn't match expected type: [a0_a26AY[tau:1]]
--                               -> Pile a_a26AR[sk:1]
--             with actual type: (a1_a26AU[tau:1], Pile a1_a26AU[tau:1])
-- Possible cause: `pop' is applied to too many arguments
-- In the second argument of `(.)', namely `pop 5'
-- In the expression: (push 5 . pop 5) []
-- In an equation for `it_a26zr': it_a26zr = (push 5 . pop 5) []
-- Relevant bindings include
--   it_a26zr :: ((), Pile a_a26AR[sk:1])
--     (bound at D:\New\fac\M1\S2\PAF\TD\td8.hs:154:2)

--- : Exo 2
newtype Compteur = C Integer deriving Show 
incr :: Compteur -> (Integer,Compteur)
incr (C n) = (n,C (n+1))

decr :: Compteur -> (Integer,Compteur)
decr (C n) = (n,C (n-1))

data Memoire a = MM {
                    pile :: Pile a, 
                    compteur :: Compteur} deriving Show

mpop :: Memoire a -> (a,Memoire a)
mpop (MM p c) = let (val,nouvelle_pile) = pop p in (val,MM nouvelle_pile c)

mpush :: a -> Memoire a -> ((),Memoire a)
mpush x (MM p c) = let (val,p') = push x p in (val,MM p' c)

mincr :: Memoire a -> (Integer,Memoire a)
mincr (MM p c) = let (val,c') = incr c in (val,MM p c')

mdecr :: Memoire a -> (Integer,Memoire a)
mdecr (MM p c) = let (val,c') = decr c in (val,MM p c')


newtype Etat s a = E { etat :: s -> (a, s) }

type EtatP a b = Etat (Pile a) b
type EtatC b = Etat Compteur b
type EtatM a b = Etat (Memoire a) b

pops :: EtatP a a
pops = E pop

pushs :: a -> EtatP a ()
pushs  = E . push 
-- ou bien x = E (push x)

incrs :: EtatC Integer
incrs = E incr

decrs :: EtatC Integer
decrs = E decr

mpushs:: a -> EtatM a ()
mpushs  = E . mpush 

mpops :: EtatM a a
mpops = E mpop

instance Functor (Etat s) where
    fmap f (E calcul) = E (\s -> let (a,s') = calcul s in (f a,s'))
instance Applicative (Etat s) where
    pure v = E (\s -> (v,s))
    (<*>) (E f)  (E a) = E (\e ->
                                let (resf,ne) = f e in 
                                let (resa,nna) = a ne in 
                                    (resf resa,nna)
                            )

mincrs :: EtatM a Integer
mincrs = E mincr

mdecrs :: EtatM a Integer
mdecrs = E mdecr

instance Monad (Etat s) where
    (>>=) (E a) f = E (\s -> let (res,s') = a s 
                        in let E r = f res in
                            r s'
                            )

stackManip :: EtatP Int Int
stackManip = do
                pushs 1
                pushs 2
                pushs 3
                x <- pops
                return x

-- >>> (etat stackManip) []
-- (3,[2,1])
