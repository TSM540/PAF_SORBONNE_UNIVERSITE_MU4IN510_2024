module Poly where

import Data.Map
import qualified Data.Map as Map

import Data.List
import qualified Data.List as List

data Poly a = Nul | Poly (Int, Map Int a)

prop_PolyInv :: (Eq a, Num a) => Poly a -> Bool
prop_PolyInv Nul = True
prop_PolyInv (Poly (d, m)) = fst (findMax m) == d && snd (findMax m) /= 0

showPoly :: (Show a, Eq a, Num a) => Poly a -> String
showPoly Nul = "null"
showPoly (Poly (d, m)) = foldrWithKey (\k v str -> str ++ show v ++ (if k > 0 then ".x^" ++ show k ++ " + " else "")) "" m

instance (Show a, Num a, Eq a) => Show (Poly a) where
    show = showPoly

-- X^4 + 2 X^2 + 4
exemple :: Poly Int
exemple = Poly (4, fromList [(0, 4), (1, 0), (2, 2), (3, 0), (4, 1)])

-- >>> exemple
-- 1.x^4 + 0.x^3 + 2.x^2 + 0.x^1 + 4

-- >>> prop_PolyInv exemple
-- True

exemple' :: Poly Int
exemple' = Poly (4, fromList [(0, 0), (1, 0), (2, 2), (3, 0), (5, 1)])

-- >>> prop_PolyInv exemple'
-- False

exemple'' :: Poly Int
exemple'' = Poly (4, fromList [(0, 0), (1, 0), (2, 2), (3, 0), (4, 0)])

-- >>> prop_PolyInv exemple''
-- False

canonise :: (Eq a, Num a) => Poly a -> Poly a
canonise Nul = Nul
canonise (Poly (d, m)) =
    let m' = Map.filter ( /= 0) m in
        Poly (fst (findMax m'), m')

-- >>> canonise exemple
-- 1.x^4 + 2.x^2 + 4

addPoly :: (Eq a, Num a) => Poly a -> Poly a -> Poly a
addPoly Nul (Poly a) = Poly a
addPoly (Poly a) Nul = Poly a
addPoly (Poly (d1, m1)) (Poly (d2, m2)) =
    let m3 = unionWith (+) m1 m2
        in canonise $ Poly (0, m3)

-- X^5 + X^4 + 5.X^3 + 2.X^3 + 2
exemple2 :: Poly Int
exemple2 = Poly (5, fromList [(0, 2), (1, 0), (2, 3), (3, 5), (4, 1), (5, 1)])

-- >>> addPoly exemple exemple2
-- 1.x^5 + 2.x^4 + 5.x^3 + 5.x^2 + 6

pmap :: (a -> b) -> Poly a -> Poly b
pmap f Nul = Nul
pmap f (Poly (d, m)) = Poly (d, Map.map f m)

negatePoly :: (Eq a, Num a) => Poly a -> Poly a
negatePoly = pmap negate
--negatePoly Nul = Nul
--negatePoly (Poly (d, m)) = Poly (d, mapKeys negate m)

subPoly :: (Eq a, Num a) => Poly a -> Poly a -> Poly a
subPoly Nul (Poly a) = negatePoly (Poly a)
subPoly (Poly a) Nul = Poly a
subPoly (Poly (d1, m1)) (Poly (d2, m2)) =
    let m3 = unionWith (-) m1 m2
        in canonise $ Poly (0, m3)

-- >>> pmap (*3) $ canonise exemple
-- 3.x^4 + 6.x^2 + 12

combinations :: Map Int a -> Map Int a -> [((Int, a), (Int, a))]
combinations m1 m2 = [(x, y) | x <- toList m1, y <- toList m2 ]

-- >>> combinations (fromList [(0, 4), (2, 2), (4, 1)]) (fromList [(0, 4), (2, 2), (4, 1)])
-- [((0,4),(0,4)),((0,4),(2,2)),((0,4),(4,1)),((2,2),(0,4)),((2,2),(2,2)),((2,2),(4,1)),((4,1),(0,4)),((4,1),(2,2)),((4,1),(4,1))]

times :: (Eq a, Num a) => [((Int, a), (Int, a))] -> [(Int, a)]
times = List.map (\elem -> (fst (fst elem) + fst (snd elem), snd (fst elem) * snd (snd elem)))

-- >>> times [((0,4),(0,4)),((0,4),(2,2)),((0,4),(4,1)),((2,2),(0,4)),((2,2),(2,2)),((2,2),(4,1)),((4,1),(0,4)),((4,1),(2,2)),((4,1),(4,1))]
-- [(0,16),(2,8),(4,4),(2,8),(4,4),(6,2),(4,4),(6,2),(8,1)]


timesPoly :: (Eq a, Num a) => Poly a -> Poly a -> Poly a
timesPoly Nul _ = Nul
timesPoly _ Nul = Nul
timesPoly (Poly (d1, m1)) (Poly (d2, m2)) = 
    canonise (Poly (d1 + d2, 
                    Map.fromListWith (+) (times $ combinations m1 m2) ))

-- >>> timesPoly exemple exemple
-- 1.x^8 + 4.x^6 + 12.x^4 + 16.x^2 + 16

absPoly :: (Eq a, Num a) => Poly a -> Poly a
absPoly = pmap abs

-- >>> canonise $ negatePoly exemple
-- -1.x^4 + -2.x^2 + -4

-- >>> absPoly $ canonise $ negatePoly exemple
-- 1.x^4 + 2.x^2 + 4

signumPoly :: (Eq a, Num a) => Poly a -> Poly a
signumPoly = id

fromIntegerPoly :: (Eq a, Num a) => Integer -> Poly a
fromIntegerPoly x 
    | x >= 0 = Poly (fromIntegral x, fromList (zip (Prelude.take (fromIntegral x+1) (iterate (+1) 0)) (Prelude.take (fromIntegral x+1) (repeat 1)) ) )
    | otherwise = error "cannot create polynome with a negative degree"

-- >>> fromIntegerPoly 4
-- 1.x^4 + 1.x^3 + 1.x^2 + 1.x^1 + 1

-- >>> fromIntegerPoly (-4)
-- cannot create polynome with a negative degree

instance (Eq a, Num a) => Num (Poly a) where
  (+) = addPoly
  (*) = timesPoly
  abs = absPoly
  signum = signumPoly
  fromInteger = fromIntegerPoly
  negate = negatePoly

deriveMap ::  Map Int Int -> Map Int Int
deriveMap m = Map.fromList (Map.foldrWithKey (\key value res -> if key > 0 then (key-1, value*key) : res else res ) [] m)

-- (4 + 2x^2 + x^4)' == 4x + 4x^3
-- >>> deriveMap (Map.fromList [(0, 4), (1, 0), (2, 2), (3, 0), (4, 1)])
-- fromList [(0,0),(1,4),(2,0),(3,4)]

derive :: Poly Int -> Poly Int
derive Nul = Nul
derive (Poly (d, m)) = canonise (Poly (d-1, deriveMap m))

-- >>> derive exemple
-- 4.x^3 + 4.x^1 + 
