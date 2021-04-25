module Chapter3 
    ( cons
    , snoc
    , fromSL
    , consSL
    , snocSL
    , tailSL
    , nullSL
    , initSL
    , headSL
    , lastSL
    , dropWhileSL
    , initsSL
    , fetch
    , RAList
    , Tree(..)
    , Digit(..)
    , fromRA
    , fetchRA
    , consRA
    ) where

import Chapter1 (single)
import Data.List (drop)

-- SYMMETRIC LISTS

type SymList a = ([a], [a])
-- null xs => null xs or single xs
-- null ys => null xs or single ys

cons :: a -> [a] -> [a]
cons x xs = x : xs

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

fromSL :: SymList a -> [a]
fromSL (xs, ys) = xs ++ reverse ys

snocSL :: a -> SymList a -> SymList a
snocSL x (xs, ys) = if null xs then (ys, [x]) else (xs , x : ys)

lastSL :: SymList a -> a
lastSL (xs, ys) = if null ys then head xs else head ys

tailSL :: SymList a -> SymList a
tailSL (xs, ys)
    | null xs   = if null ys then undefined else nilSL
    | single xs = (reverse vs, us)
    | otherwise = (tail xs, ys)
    where (us, vs) = splitAt (length ys `div` 2) ys

-- Exercise 3.2

nilSL :: SymList a
nilSL = ([], [])

nullSL :: SymList a -> Bool
nullSL (xs, ys) = null xs && null ys

singleSL :: SymList s -> Bool
singleSL (xs, ys) = single xs && null ys || null xs && single ys

lengthSL :: SymList a -> Int
lengthSL (xs, ys) = length xs + length ys

-- Exercise 3.3

consSL :: a -> SymList a -> SymList a
consSL x (xs, ys) = if null ys then ([x], xs) else (x : xs, ys)

headSL :: SymList a -> a
headSL (xs, ys) = if null xs then head ys else head xs

-- Exercise 3.4

initSL :: SymList a -> SymList a
initSL (xs, ys)
    | null ys   = if null xs then undefined else nilSL
    | single ys = (vs, reverse us)
    | otherwise = (xs, tail ys)
    where (vs, us) = splitAt (length xs `div` 2) xs

-- Exercise 3.5

-- dropWhile . fromSL = fromSL . dropWhileSL

dropWhileSL :: (a -> Bool) -> SymList a -> SymList a
dropWhileSL p (xs, ys)
    | null ys   = (dropWhile p xs, [])
    | single ys = if null xs' then ([], dropWhile p ys) else (xs', ys)
    | otherwise = if null xs' then (xs'', ys'') else (xs', ys)
    where (_, xs') = span p xs
          ys'  = dropWhile p (reverse ys)
          (xs'', ys'') = if null ys' then nilSL else ([head ys'], reverse (tail ys'))

-- Solution:
dropWhileSL' :: (a -> Bool) -> SymList a -> SymList a
dropWhileSL' p sl
    | nullSL sl     = nilSL
    | p (headSL sl) = dropWhileSL' p (tailSL sl)
    | otherwise     = sl 

-- Exercise 3.6

-- inits . fromSL = map fromSL . fromSL . initsSL 

{-}
inits' :: [a] -> [[a]]
inits' = foldr (\x y -> [] : map (x:) y) [[]]

foldr :: (b -> a -> b) -> b -> [a] -> [b]
foldr f z [] = z
foldr f (x : xs) = f x (foldr f z xs)
-}

initsSL :: SymList a -> SymList (SymList a)
initsSL sl = if nullSL sl
             then consSL nilSL nilSL
             else op (headSL sl) (initsSL (tailSL sl))
                where op sl slsl = consSL nilSL (mapSL (consSL sl) slsl)
                      mapSL f (xs, ys) = (map f xs, map f ys)

-- Solution:
initsSL' :: SymList a -> SymList (SymList a)
initsSL' sl = if nullSL sl
              then snocSL sl nilSL
              else snocSL sl (initsSL (initSL sl))

-- Exercise 3.7

inits' :: [a] -> [[a]]
inits' = map reverse . scanl (flip (:)) []

-- RANDOM-ACCESS LISTS

fetch :: Int -> [a] -> a
fetch k xs = if k == 0 then head xs else fetch (k - 1) (tail xs)

data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving Show

size :: Tree a -> Int
size (Leaf _)     = 1
size (Node n _ _) = n

node :: Tree a -> Tree a -> Tree a
node t1 t2 = Node (size t1 + size t2) t1 t2

data Digit a = Zero | One (Tree a) deriving Show
type RAList a = [Digit a]

fromRA :: RAList a -> [a]
fromRA = concatMap from
         where from Zero = []
               from (One t) = fromT t

fromT :: Tree a -> [a]
fromT (Leaf x)       = [x]
fromT (Node _ t1 t2) = fromT t1 ++ fromT t2

fetchRA :: Int -> RAList a -> a
fetchRA k (Zero : xs)  = fetchRA k xs
fetchRA k (One t : xs) = if k < size t 
                         then fetchT k t 
                         else fetchRA (k - size t) xs

fetchT :: Int -> Tree a -> a
fetchT 0 (Leaf x) = x
fetchT k (Node n t1 t2) = if k < m 
                          then fetchT k t1 
                          else fetchT (k - m) t2
                          where m = n `div` 2

consRA :: a -> RAList a -> RAList a
consRA x xs = consT (Leaf x) xs

consT :: Tree a -> RAList a -> RAList a
consT t1 []            = [One t1]
consT t1 (Zero : xs)   = One t1 : xs
consT t1 (One t2 : xs) = Zero : consT (node t1 t2) xs

unconsRA :: RAList a -> (a, RAList a)
unconsRA xs = (x, ys) where (Leaf x, ys) = unconsT xs

unconsT :: RAList a -> (Tree a, RAList a)
unconsT (One t : xs) = if null xs then (t, []) else (t, Zero : xs)
unconsT (Zero : xs)  = (t1, One t2 : ys) where (Node _ t1 t2, ys) = unconsT xs

