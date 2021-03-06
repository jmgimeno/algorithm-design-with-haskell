module Chapter1

import Control.Algebra
import Syntax.PreorderReasoning

easy : (xs : List a)
        -> (ys : List a)
        -> (f : a -> b -> b)
        -> (e : b)
        -> foldr f e (xs ++ ys) = foldr f (foldr f e ys) xs
easy [] ys f e = Refl
easy (x :: xs) ys f e 
  = rewrite easy xs ys f e in Refl

master : (xs : List a)
        -> (h : b -> c)
        -> (f : a -> b -> b)
        -> (e : b)
        -> (g : a -> c -> c)
        -> ((x: a) -> (y: b) -> h (f x y) = g x (h y))
        -> h (foldr f e xs) = foldr g (h e) xs
master [] h f e g proviso = Refl
master (x :: xs) h f e g proviso 
  = let prov = proviso x (foldr f e xs) in 
        rewrite prov in 
                let ih = master xs h f e g proviso in
                    rewrite ih in 
                            Refl

-- Exercise 1.10

rev : (xs : List a) -> List a
rev [] = []
rev (x :: xs) = rev xs ++ [x]

lemma : (xs : List a)
                -> (op : a -> a -> a)
                -> (e : a)
                -> foldl op e xs = foldr (flip op) e (rev xs)
lemma [] op e = Refl
lemma (x :: xs) op e = let ih = lemma xs op (op e x) in
                       rewrite ih in 
                       let ea = easy (rev xs) [x] (flip op) e in
                       rewrite ea in 
                       Refl
                       
cong3 : (0 g : t1 -> t2 -> t3 -> u)
          -> (p1 : a = b)
          -> (p2 : c = d)
          -> (p3 : e = f)
          -> g a c e = g b d f
cong3 g Refl Refl Refl = Refl

lemma_ex_1_10 : (xs : List a)
                -> (y : a)
                -> (op : a -> a -> a)
                -> (e : a)
                -> (unitL: (x : a) -> op e x = x)
                -> (unitR: (x : a) -> op x e = x)
                -> (assoc: (x, y, z : a) -> op (op x y) z = op x (op y z))
                -> foldl op y xs = op y (foldr op e xs)
lemma_ex_1_10 [] y op e unitL unitR assoc 
  = sym $ unitR _
lemma_ex_1_10 (x :: xs) y op e unitL unitR assoc 
  = rewrite lemma_ex_1_10 xs (op y x) op e unitL unitR assoc in 
    assoc _ _ _

ex1_10 : (xs : List a)
                -> (op : a -> a -> a)
                -> (e : a)
                -> (unitL: (x : a) -> op e x = x)
                -> (unitR: (x : a) -> op x e = x)
                -> (assoc: (x, y, z : a) -> op (op x y) z = op x (op y z))
                -> foldl op e xs = foldr op e xs
ex1_10 xs op e unitL unitR assoc 
  = rewrite lemma_ex_1_10 xs e op e unitL unitR assoc in
    unitL _    


