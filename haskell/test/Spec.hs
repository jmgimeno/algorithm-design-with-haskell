import Chapter1 (ex1_6, ex1_6')

import Test.QuickCheck
import Test.QuickCheck.Function

prop_ex16 :: Eq b => Fun a Bool -> Fun (a, b) b -> b -> [a] -> Bool
prop_ex16 (Fn p) (Fn2 f) e xs = ex1_6 p f e xs == ex1_6' p f e xs

main = quickCheck (prop_ex16 :: Fun Int Bool -> Fun (Int, Int) Int -> Int -> [Int] -> Bool)
