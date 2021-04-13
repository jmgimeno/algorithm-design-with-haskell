module Chapter1 where

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