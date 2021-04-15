import Chapter1

import Test.QuickCheck
import Test.QuickCheck.Function

import Data.List (takeWhile, dropWhileEnd)

prop_ex1_6 :: Eq b => Fun a Bool -> Fun (a, b) b -> b -> [a] -> Bool
prop_ex1_6 (Fn p) (Fn2 f) e xs = ex1_6 p f e xs == ex1_6' p f e xs

prop_ex1_7 :: Eq a => Fun a Bool -> [a] -> Bool
prop_ex1_7 (Fn p) xs = takeWhile' p xs == takeWhile p xs

prop_ex1_8 :: Eq a => Fun a Bool -> [a] -> Bool
prop_ex1_8 (Fn p) xs = dropWhileEnd' p xs == dropWhileEnd p xs

main = do
    quickCheck (prop_ex1_6 :: Fun Int Bool -> Fun (Int, Int) Int -> Int -> [Int] -> Bool)
    quickCheck (prop_ex1_7 :: Fun Int Bool -> [Int] -> Bool)
    quickCheck (prop_ex1_8 :: Fun Int Bool -> [Int] -> Bool)
