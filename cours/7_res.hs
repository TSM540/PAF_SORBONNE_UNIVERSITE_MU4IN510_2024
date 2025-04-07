-- Applicative function

class Functor f => Applicative (f :: * -> *) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
--   liftA2 :: (a -> b -> c) -> f a -> f b -> f c
--   (*>) :: f a -> f b -> f b
--   (<*) :: f a -> f b -> f a
  {-# MINIMAL pure, ((<*>) | liftA2) #-}

fmap :: (a -> b) -> f a -> f b
 fmap putStrLn getLine -- read one line from standard input and 
 putStrLn <$> getLine -- print it to standard output
-- if we want to read multiple lines this code can no longer be used (<$>)
-- we need to use the applicative function
-- (<$>) :: (a -> b) -> f a -> f b
-- (<*>) :: f (a -> b) -> f a -> f b
  putStrLn <$> ((++) <$> getLine <*> getLine)
  -- and it works because the operator for applicative is equivalent to 
instance Applicative IO where 
        pure = return
        a <*> b = do    
            f <- a
            x <- b
            return (f x)

-- ### • pure permet d'injecter une valeur dans le contexte  (arité 0)
-- ### • <$> correspond à l'arité 1
-- ### • <*> se nomme «apply»  pour l'«application contextuelle»  (arité >1)

-- en résumé :
-- <$> est utilisé pour appliquer une fonction à un seul argument contenu dans un foncteur.
-- <*> est utilisé pour appliquer une fonction à deux arguments contenus dans deux foncteurs différents.
 -- exemple : 
f :: Int -> String
<$> f (Just 5) -- marche car il y a une seul arguement mais pas avec <*>

g :: Int -> Int -> String
(Just 5) <*> (Just 10) <*> g
-- Les lois applicatives :
-- Identité : pure id <*> v = v
-- Homomorphisme : pure g <*> pure x = pure (g x)
-- Interchange : u <*> pure y = pure ($ y) <*> u  // ou bien
            -- u <*> pure y = pure (\f -> f y) <*> u
-- Fonctorielle : f <$> x = pure f <*> x

data MyMaybe a =
  MyNothing
  | MyJust a
  deriving (Show, Eq, Ord)


-- # Etape 1 : instanciation de Functor

instance Functor MyMaybe where
  fmap :: (a -> b) -> MyMaybe a -> MyMaybe b
  fmap _ MyNothing = MyNothing
  fmap g (MyJust x) = MyJust (g x)



-- # Etape 2 : instanciation de Applicative
-- ### pure injecte une valeur «simple» dans le contexte

pureMyMaybe :: a -> MyMaybe a
pureMyMaybe = MyJust



applyMyMaybe :: MyMaybe (a -> b) -> MyMaybe a -> MyMaybe b
applyMyMaybe (MyJust g) (MyJust x) = MyJust (g x)
applyMyMaybe _ _ = MyNothing
instance Applicative MyMaybe where
  pure = pureMyMaybe
  (<*>) = applyMyMaybe

-- les listes
data List a =
  Nil
  | Cons a (List a)
  deriving (Show, Eq, Ord)
-- # Etape 1 : instanciation de Functor


instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Nil = Nil
  fmap g (Cons x xs) = Cons (g x) $ fmap g xs

-- # Etape 2 : instanciation de Applicative
pureList :: a -> List a
pureList x = Cons x Nil
applyList :: List (a -> b) -> List a -> List b

appendList :: List a -> List a -> List a
appendList Nil ys = ys
appendList (Cons x xs) ys = Cons x $ appendList xs ys

concatList :: List (List a) -> List a
concatList Nil = Nil
concatList (Cons xs yys) = xs `appendList` (concatList yys)

concatMapList :: (a -> List b) -> List a -> List b
concatMapList g = concatList . fmap g

applyList gs xs = concatMapList (\g -> fmap g xs) gs

instance Applicative List where
  pure = pureList
  (<*>) = applyList


-- (.) is function composition: if you have g :: a -> b and f :: b -> c then f . g is essentially f(g(x)): first use g on an a to get a b and then use f on that b to get a c

-- <$> takes a function taking an a and returning a b, and a functor that contains an a, and it returns a functor that contains a b. So <$> is the same as fmap :: (a -> b) -> f a -> f b

-- <*> takes a functor that contains a function taking an a and returning a b, and a functor that contains an a, and it returns a functor that contains a b. So <*> kind of extract the function from a functor and applies it to an arguments also inside a functor, and finally returns the result into a functor