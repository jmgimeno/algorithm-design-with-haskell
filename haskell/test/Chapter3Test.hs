module Chapter3Test where
import Chapter3

import Test.QuickCheck
import Test.QuickCheck.Function

import Data.List (dropWhile, inits)

normalize :: [a] -> [a] -> ([a], [a])
normalize [] (y : ys) = ([y], ys)
normalize (x : xs) [] = (xs, [x])
normalize xs ys       = (xs, ys)

-- cons x . fromSL = fromSL . consSL x
prop_eq1 :: Eq a => a -> [a] -> [a] -> Bool
prop_eq1 x xs ys = cons x (fromSL sl) == fromSL (consSL x sl)
                   where sl = normalize xs ys

-- snoc x . fromSL = fromSL . snocSL x
prop_eq2 :: Eq a => a -> [a] -> [a] -> Bool
prop_eq2 x xs ys = snoc x (fromSL sl) == fromSL (snocSL x sl)
                   where sl = normalize xs ys

-- tail . fromSL = fromSL . tailSL
prop_eq3 :: Eq a => [a] -> [a] -> Bool
prop_eq3 xs ys = nullSL sl || tail (fromSL sl) == fromSL (tailSL sl)
                   where sl = normalize xs ys

-- init . fromSL = fromSL . initSL
prop_eq4 :: Eq a => [a] -> [a] -> Bool
prop_eq4 xs ys = nullSL sl || init (fromSL sl) == fromSL (initSL sl)
                   where sl = normalize xs ys

-- head . fromSL = headSL
prop_eq5 :: Eq a => [a] -> [a] -> Bool
prop_eq5 xs ys = nullSL sl || head (fromSL sl) == headSL sl
                   where sl = normalize xs ys
                   
-- last . fromSL = lastSL
prop_eq6 :: Eq a => [a] -> [a] -> Bool
prop_eq6 xs ys = nullSL sl || (last (fromSL sl) == lastSL sl)
                   where sl = normalize xs ys

-- dropWhile . fromSL = fromSL . dropWhileSL
prop_dropWhileSL :: Eq a => Fun a Bool -> [a] -> [a] -> Bool
prop_dropWhileSL (Fn p) xs ys = dropWhile p (fromSL sl) == fromSL (dropWhileSL p sl)
                                where sl = normalize xs ys

-- inits . fromSL = map fromSL . fromSL . initsSL 
prop_initsSL :: Eq a => [a] -> [a] -> Bool
prop_initsSL xs ys = inits (fromSL sl) == map fromSL (fromSL (initsSL sl))
                     where sl = normalize xs ys

chapter3Tests :: IO ()
chapter3Tests = do
    quickCheck (prop_eq1 :: Int -> [Int] -> [Int] -> Bool)
    quickCheck (prop_eq2 :: Int -> [Int] -> [Int] -> Bool)
    quickCheck (prop_eq3 :: [Int] -> [Int] -> Bool)
    quickCheck (prop_eq4 :: [Int] -> [Int] -> Bool)
    quickCheck (prop_eq5 :: [Int] -> [Int] -> Bool)
    quickCheck (prop_eq6 :: [Int] -> [Int] -> Bool)
    quickCheck (prop_dropWhileSL :: Fun Int Bool -> [Int] -> [Int] -> Bool)
    quickCheck (prop_initsSL :: [Int] -> [Int] -> Bool)
