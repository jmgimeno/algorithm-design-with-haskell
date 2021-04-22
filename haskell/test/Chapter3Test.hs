module Chapter3Test where
import Chapter3

import Test.QuickCheck
import Test.QuickCheck.Function

import Data.List (dropWhile)

-- dropWhile . fromSL = fromSL . dropWhileSL

normalize :: [a] -> [a] -> ([a], [a])
normalize [] (y : ys) = ([y], ys)
normalize (x : xs) [] = (xs, [x])
normalize xs ys       = (xs, ys)

prop_dropWhileSL :: Eq a => Fun a Bool -> [a] -> [a] -> Bool
prop_dropWhileSL (Fn p) xs ys = dropWhile p (fromSL sl) == fromSL (dropWhileSL p sl)
                                where sl = normalize xs ys

chapter3Tests :: IO ()
chapter3Tests = quickCheck (prop_dropWhileSL :: Fun Int Bool -> [Int] -> [Int] -> Bool)
