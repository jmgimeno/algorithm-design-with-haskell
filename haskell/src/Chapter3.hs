module Chapter3 
    ( fromSL
    , dropWhileSL
    ) where

import Chapter1 (single)
import Data.List (drop)

type SymList a = ([a], [a])
-- null xs => null xs or single xs
-- null ys => null xs or single ys

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

