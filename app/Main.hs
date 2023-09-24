module Main where

import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  quickCheck prop_nAnd
  quickCheck prop_nAnd_1
  quickCheck prop_nAnd_2
  quickCheck prop_nAnd_3

-- 1
nAnd :: Bool -> Bool -> Bool
nAnd x y = not (x && y)

nAnd_1 :: Bool -> Bool -> Bool
nAnd_1 True x = not x
nAnd_1 False x = True

nAnd_2 :: Bool -> Bool -> Bool
nAnd_2 False False = True
nAnd_2 False True = True
nAnd_2 True False = True
nAnd_2 True True = False

-- 2
prop_nAnd :: Bool -> Bool -> Bool
prop_nAnd x y = nAnd x y == not (x && y)

prop_nAnd_1 :: Bool -> Bool -> Bool
prop_nAnd_1 x y = nAnd_1 x y == not (x && y)

prop_nAnd_2 :: Bool -> Bool -> Bool
prop_nAnd_2 x y = nAnd_2 x y == not (x && y)

prop_nAnd_3 :: Bool -> Bool -> Bool
prop_nAnd_3 False x = nAnd False x == True
prop_nAnd_3 x False = nAnd x False == True
prop_nAnd_3 True True = nAnd True True == False