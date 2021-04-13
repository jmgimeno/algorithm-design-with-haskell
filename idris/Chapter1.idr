module Chapter1

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
