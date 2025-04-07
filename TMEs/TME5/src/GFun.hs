module GFun where

data Serie a = Z a (Serie a)

-- >>> :t Z
-- Z :: forall a. a -> Serie a -> Serie a

uns :: Serie Integer 
uns = Z 1 uns

prend :: Int -> Serie a -> [a]
prend 0 _ = []
prend x (Z a reste) 
    | x > 0     = a : prend (x-1) reste
    | otherwise = error "cannot take a negative number of element from serie"

-- >>> prend 11 uns
-- [1,1,1,1,1,1,1,1,1,1,1]

-- >>> prend 0 uns
-- []

-- >>> prend (-2) uns
-- cannot take a negative number of element from serie

-- >>> zip [0..11] (prend 11 uns)
-- [(0,1),(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(9,1),(10,1)]

showSerie :: (Show a) => Serie a -> String
showSerie s = foldl (\str elem -> if fst elem < 10 then 
                                        str ++ show (snd elem) ++ ".z^" ++ show (fst elem) ++ " + " 
                                    else str ++ "...") 
                    "" 
                    (zip [0..11] (prend 11 s))

-- >>> showSerie uns
-- "1.z^0 + 1.z^1 + 1.z^2 + 1.z^3 + 1.z^4 + 1.z^5 + 1.z^6 + 1.z^7 + 1.z^8 + 1.z^9 + ..."

instance (Show a) => Show (Serie a) where
    show = showSerie

-- >>> show uns
-- "1.z^0 + 1.z^1 + 1.z^2 + 1.z^3 + 1.z^4 + 1.z^5 + 1.z^6 + 1.z^7 + 1.z^8 + 1.z^9 + ..."

gmap :: (a -> b) -> Serie a -> Serie b
gmap f (Z a rest) = Z (f a) (gmap f rest)

-- >>> show (gmap (*2) uns)
-- "2.z^0 + 2.z^1 + 2.z^2 + 2.z^3 + 2.z^4 + 2.z^5 + 2.z^6 + 2.z^7 + 2.z^8 + 2.z^9 + ..."

instance Functor Serie where
    fmap = gmap

-- >>> show (fmap (*2) uns)
-- "2.z^0 + 2.z^1 + 2.z^2 + 2.z^3 + 2.z^4 + 2.z^5 + 2.z^6 + 2.z^7 + 2.z^8 + 2.z^9 + ..."

instance (Num a) => Num (Serie a) where
    (Z c1 s1) + (Z c2 s2) = Z (c1 + c2) (s1 + s2)
    (Z c1 s1) - (Z c2 s2) = Z (c1 - c2) (s1 - s2)
    (Z c1 s1) * (Z c2 s2) = Z (c1 * c2) (gmap (*c1) s2 + gmap (*c2) s1 + Z 0 (s1 * s2))
    abs (Z c1 s1) = Z (abs c1) (abs s1)
    signum (Z c1 s1) = Z (signum c1) (signum s1)
    fromInteger i = Z (fromInteger i) (fromInteger i)

instance (Eq a) => Eq (Serie a) where
  (Z c1 s1) == (Z c2 s2) = c1 == c2

-- >>> show (uns + uns)
-- "2.z^0 + 2.z^1 + 2.z^2 + 2.z^3 + 2.z^4 + 2.z^5 + 2.z^6 + 2.z^7 + 2.z^8 + 2.z^9 + ..."

-- >>> show (negate uns)
-- "-1.z^0 + -1.z^1 + -1.z^2 + -1.z^3 + -1.z^4 + -1.z^5 + -1.z^6 + -1.z^7 + -1.z^8 + -1.z^9 + ..."

-- >>> show ((gmap (*2) uns) * (gmap (*2) uns))
-- "4.z^0 + 8.z^1 + 12.z^2 + 16.z^3 + 20.z^4 + 24.z^5 + 28.z^6 + 32.z^7 + 36.z^8 + 40.z^9 + ..."

-- >>> show (fromInteger 4 :: Serie Int)
-- "4.z^0 + 4.z^1 + 4.z^2 + 4.z^3 + 4.z^4 + 4.z^5 + 4.z^6 + 4.z^7 + 4.z^8 + 4.z^9 + ..."

-- >>> show (uns + fromInteger 4)
-- "5.z^0 + 5.z^1 + 5.z^2 + 5.z^3 + 5.z^4 + 5.z^5 + 5.z^6 + 5.z^7 + 5.z^8 + 5.z^9 + ..."

-- >>> show (signum $ negate (fromInteger 4 :: Serie Int)) 
-- "-1.z^0 + -1.z^1 + -1.z^2 + -1.z^3 + -1.z^4 + -1.z^5 + -1.z^6 + -1.z^7 + -1.z^8 + -1.z^9 + ..."

derive :: Num a => Serie a -> Serie a
derive (Z a rest) = aux (Z a rest) 1 where
    aux (Z c s) counter = Z (c * counter) (aux s (counter + 1))

-- >>> show (derive uns)
-- "1.z^0 + 2.z^1 + 3.z^2 + 4.z^3 + 5.z^4 + 6.z^5 + 7.z^6 + 8.z^7 + 9.z^8 + 10.z^9 + ..."
