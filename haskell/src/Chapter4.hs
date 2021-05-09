module Chapter4 where

type Nat = Int

-- ONE-DIMENSIONAL SEARCH PROBLEM

linearSearch :: (Nat -> Nat) -> Nat -> [Nat]
linearSearch f t = seek (0, t)
  where
    seek (a, b) = [x | x <- [a .. b], t == f x]

search :: (Nat -> Nat) -> Nat -> [Nat]
search f t = seek (0, t)
  where
    seek (a, b)
      | a > b = []
      | t < f m = seek (a, m -1)
      | t == f m = [m]
      | otherwise = seek (m + 1, b)
      where
        m = choose (a, b)
    choose (a, b) = (a + b) `div` 2

bound :: (Nat -> Nat) -> Nat -> (Int, Nat)
bound f t = if t <= f 0 then (-1, 0) else (b `div` 2, b)
  where
    b = until done (* 2) 1
    done b = t <= f b

search' :: (Nat -> Nat) -> Nat -> [Nat]
search' f t = [x | x == t]
  where
    x = smallest (bound f t) f t

smallest :: (Nat, Nat) -> (Nat -> Nat) -> Nat -> Nat
smallest (a, b) f t
  | a + 1 == b = b
  | t <= f m = smallest (a, m) f t
  | otherwise = smallest (m, b) f t
  where
    m = (a + b) `div` 2
