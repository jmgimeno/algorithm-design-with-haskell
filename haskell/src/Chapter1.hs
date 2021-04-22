module Chapter1 
    ( ex1_6
    , ex1_6'
    , takeWhile'
    , dropWhileEnd'
    , single
    ) where

import Data.List (inits, tails)

head' :: [a] -> a
head' = foldr const (error "head of empty")

perms1 :: [a] -> [[a]]
perms1 [] = [[]]
perms1 (x : xs) = [zs | ys <- perms1 xs, zs <- inserts x xs]

inserts :: a -> [a] -> [[a]]
inserts x [] = [[x]]
inserts x (y : ys) = (x : y : ys) : map (y:) (inserts x ys)

perms1' :: [a] -> [[a]]
--perm1' = foldr step [[]] where step x xss = concatMap (inserts x) xss
perms1' = foldr (concatMap . inserts) [[]]

perms2 :: [a] -> [[a]]
perms2 [] = [[]]
perms2 xs = [x:zs | (x,ys) <- picks xs, zs <- perms2 ys]

picks :: [a] -> [(a, [a])]
picks [] = []
picks (x : xs) = (x, xs) : [(y, x:ys) | (y,ys) <- picks xs]

perms2' :: [a] -> [[a]]
perms2' [] = [[]]
perms2' xs = concatMap subperms (picks xs)
                where
                    subperms (x, ys) = map (x:) (perms2' ys)

{-

* Master fusion law of foldr: 

    h(foldr f e xs) = foldr g (h e) xs

  for all finite list provided that:

    h(f x y) = g x (h y)

* Easy problem:

    foldr f e (xs ++ ys) = foldr f (foldr f e ys) xs

* Difficult problem:

    foldr f e . concat = foldr (flip (foldr f)) e

-}

{-
    foldr f e . concat = ???

    - concat = foldr (++) []
    - applying master fusion rule for foldr

        h(foldr (++) [] xss) = foldr g (h []) xss
            where f = (++), e = []
        provided that
            h(xs ++ ys) = g xs (h ys)
        but h = foldr f e
            foldr f e (hs ++ ys)
                = foldr f (foldr f e ys) xs
                = foldr f (h ys) xs
                = g xs (h ys)
                => g = flip (foldr f)
-}

fun :: (a -> b -> b) -> b -> [[a]] -> b
fun f e = foldr f e . concat

fun' :: (a -> b -> b) -> b -> [[a]] -> b
fun' f e = foldr (flip (foldr f)) e

-- Exercise 1.2

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing 
uncons (x : xs) = Just (x, xs)

-- Exercise 1.3

wrap :: a -> [a]
wrap x = [x]

unwrap :: [a] -> a
unwrap [x] = x

single :: [a] -> Bool
single [_] = True
single _   = False

-- Exercise 1.4

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

-- Exercise 1.5

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

-- Exercise 1.6

{-
    foldr f e . filter p as an instance of filter

        - h = foldr f e

        - h(filter p xs) = h(foldr (\x acc -> if p x then x : acc else acc) [] xs)

            f x y = if p x then x : y else y

            h(f x y) = g x (h y)  

            h(if p x then x : y else y) = g x (h y) 

        - p x => h (x : y) = g x (h y)
                 h (x : y) = foldr f e (x : y) 
                           = f x (foldr f e y) 
                           = f x (h y)
                           = g x (h y)
                           => g = f = \x hy = f x hy

        - otherwise => h y = g x (h y) => g = \ _ hy -> hy

        => g x y = if p x then f x y else y
-}

ex1_6 :: (a -> Bool) -> (a -> b -> b) -> b -> [a] -> b
ex1_6 p f e = foldr f e . filter p

ex1_6' :: (a -> Bool) -> (a -> b -> b) -> b -> [a] -> b
ex1_6' p f = foldr (\x hy -> if p x then f x hy else hy)

-- Exercise 1.7

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p = foldr op [] where op x xs = if p x then x : xs else []

-- Exercise 1.8

{-
  dropWhileEnd = reverse . dropWhile p . reverse

  reverse = foldr (\x y -> y ++ [x]) []

  - First fusion:

    h = dropWhile p
    f x y = \x y -> y ++ [x]

    dropWhile p (y ++ [x]) = g x (h y)

    g x hy = if (null hy) then dropWhile p [x] else hy ++ [x]

    step x []
        | p x = []
        | otherwise = [x]
    step x y = y ++ [x]

  - Second fusion:

    h = reverse
    f x y = step x y

    h (f x y) = g x hy where hy = reverse y

    step x []                   ----> y = [] <-> hy = []
        | p x = []              ----> []
        | otherwhise = [x]      ----> [x]
    step x y = y ++ [x]         ----> reverse (y ++ [x]) =
                                      [x] ++ reverse y =
                                      [x] ++ hy = x : hy
-}

dropWhileEnd' :: (a -> Bool) -> [a] -> [a]
dropWhileEnd' p = foldr op []
                    where op x []
                            | p x = []
                            | otherwise = [x]
                          op x xs = x : xs

-- Exercise 1.11

integer :: [Int] -> Int
integer = foldl (\x y -> x * 10 + y) 0

fraction :: [Int] -> Double
fraction = foldr (\x y -> (fromIntegral x + y) / 10) 0

-- Exercise 1.12

inits' :: [a] -> [[a]]
inits' = foldr (\x y -> [] : map (x:) y) [[]]

tails' :: [a] -> [[a]]
tails' = foldr (\x y -> [x : head y] ++ y) [[]]

-- map (foldl f e) . inits = ??? = scanl f e 
-- map (foldr f e) . tails = ??? = scanr f e

-- NOTE: I suppose I need an inductive proof of that

-- Exercise 1.13

apply :: Int -> (a -> a) -> a -> a
apply 0 _ z = z
apply n f z = apply (n-1) f (f z)

-- Exercise 1.14

{-
inserts :: a -> [a] -> [[a]]
inserts x [] = [[x]]
inserts x (y : ys) = (x : y : ys) : map (y:) (inserts x ys)
-}

inserts' :: a -> [a] -> [[a]]
inserts' x xs = foldr op [[x]] xs
                    where 
                        op y yss = (x : y : tail (head yss)) : map (y:) yss

-- Exercise 1.15

perms3 :: Eq a => [a] -> [[a]]
perms3 [] = [[]]
perms3 xs = [x:ys | x <- xs, ys <- perms3 (remove x xs)]

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove y (x : xs)
    | y == x = xs
    | otherwise = y : remove y xs

-- Exercise 1.20

concat' :: [[a]] -> [a]
concat' xss = foldl op id xss [] 
                where op k xs =  k . (xs ++)
                -- op :: ([a] -> [a]) -> [a] -> ([a] -> [a])

-- Exercise 1.21

steep :: (Num a, Ord a) => [a] -> Bool
steep [] = True 
steep (x : xs) = x > sum xs && steep xs

faststeep :: (Num a, Ord a) => [a] -> (Bool, a)
faststeep [] = (True, 0)
faststeep (x : xs) = let (b, s) = faststeep xs
                    in (x < s && b, x + s)

steep' :: (Num a, Ord a) => [a] -> Bool
steep' = fst . faststeep

