ones:: [Integer]
ones = 1:ones
-- Q1.1
genInteger :: Integer -> [Integer]
genInteger n = n : (genInteger n) -- genInteger n = repeat n

-- Q1.2
myTake :: Integer -> [a] -> [a]
myTake _ [] = []
myTake n (x:xs) 
    | n > 0 = x : myTake (n-1) xs
    | otherwise = []
    -- ! fonction récursive
    -- ! est stricte 
    -- ! totale 
-- Q1.3
nats:: [Integer]
nats = genNats 1
    where  genNats n = n : (genNats (n+1))

-- Q1.4

nats' :: [Integer]
nats' = 1 : map (+1) nats'
-- Q1.5
fibo :: [Integer]
fibo = genFibo 1 1 
    where
        genFibo a b = a : genFibo b (a+b)

-- Q1.6
-- zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
fib2 :: [Integer]
fib2 = 1 : 1 : zipWith (+) fib2 (tail fib2)

 
-- Ex 2
-- Q2.1
-- flatMap :: (a -> [b]) -> [a] -> [[b]] si le type de retour est [[b]] on doit utiliser le map
flatMap :: (a -> [b]) -> [a] -> [b] -- alors on doit utiliser concat pour aplatir la liste
flatMap f [] = []
flatMap f (x:xs) = (f x ) ++ (flatMap f xs)
floatMap f =  foldl step [] 
    where step x acc = foldl (:) acc (f x)

-- Q2.2
-- flip :: (a -> b -> c) -> b -> a -> c
-- flip f y x = f x y 
compr :: [a] -> (a -> [b])->[b]
compr = flip flatMap
-- a) [1, 4 , 9 , 16, 25 , 36 , 49 , 64 , 81 , 100]
-- b) [1,10,100,2,20,200,3,30,300]
-- c) [(1,1),(1,2),(1,3),(1,4),(1,5),(2,1),(2,2),(2,3)...]

-- Q2.3
success :: a -> [a]
success x = [x]

failure :: [a]
failure = []
-- a) [2,4,6,8]
-- b> [2,4,6,8,10,12,14,16,18,20]
 -- c) [(2,4),(4,16),...]

select :: (a->Bool )->(a -> b) -> a -> [b]
select p f x = if (p x) then success (f x) else failure 

-- Ex 3
data Tree a =
    Tip
    | Node a (Tree a) (Tree a)
    deriving Show
leaf :: a -> Tree a
leaf v = Node v Tip Tip
exTree :: Tree Int
    exTree = Node 5 (Node 7 (leaf 6) (leaf 4))
    (Node 2 (leaf 1) (leaf 3))
-- Q3.1
maxArbre :: Ord a => Tree a -> Maybe a
maxArbre Tip = Nothing
maxArbre (Node v Fg Fd) = 
    case (maxArbre fg, maxArbre fd) of
        (Nothing, Nothing) -> Just v
        (Just x, Nothing) -> Just (max v x)
        (Nothing, Just y) -> Just (max v y)
        (Just x, Just y) -> Just (max v (max x y))
-- Q3.2
addArbre :: Num a => a -> Tree a -> Tree a
addArbre _ Tip = Tip 
addArbre x (Node v fg fd) = Node (v+x) (addArbre x fg ) (addArbre x fd)

-- Q3.3
addMaxArbre :: (Num a, Ord a) => Tree a -> Tree a
addMaxArbre  Tip = Tip 
addMaxArbre t = case maxeArbre t of 
    Nothing -> Tip 
    Just x -> addArbre x t
