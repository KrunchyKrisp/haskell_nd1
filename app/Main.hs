module Main where

import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "1. nAnd tests:"
  print (nAnd True True)
  print (nAnd_1 True True)
  print (nAnd_2 True True)
  putStrLn "2. prop_nAnd tests:"
  quickCheck prop_nAnd
  quickCheck prop_nAnd_1
  quickCheck prop_nAnd_2
  quickCheck prop_nAnd_3
  putStrLn ""
  putStrLn "3. nDigits tests:"
  print (nDigits (-10))
  print (nDigits 100)
  putStrLn ""
  putStrLn "4. nRoots tests:"
  print (nRoots 1 0 0)
  print (nRoots 1 10 0)
  print (nRoots 1 0 1)
  --print (nRoots 0 0 0)
  putStrLn ""
  putStrLn "5. smallerRoot/largerRoot tests:"
  print (smallerRoot 1 10 0)
  print (largerRoot 1 10 0)
  putStrLn ""
  putStrLn "6. power2 tests:"
  print (power2 (-10))
  print (power2 0)
  print (power2 1)
  print (power2 2)
  print (power2 5)
  putStrLn ""
  putStrLn "7. mult tests:"
  print (mult 10 5)
  print (mult 5 (-25))
  print (mult (-5) 25)
  print (mult (-10) (-25))
  putStrLn ""
  putStrLn "8. prod tests:"
  print (prod 1 3)
  print (prod 5 6)
  print (prod (-1) 1)
  --print (prod 3 1)

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

-- 3
nDigits :: Integer -> Int
nDigits x
  | x >= 0 = y
  | otherwise = y - 1
  where y = length (show x)

-- 4
nRoots :: Float -> Float -> Float -> Int
nRoots a b c
  | a == 0 = error "the first argument should be non-zero!"
  | b2 > d = 2
  | b2 == d = 1
  | otherwise = 0
  where
    b2 = b * b
    d = 4 * a * c

-- 5
smallerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c
  | a == 0 = error "the first argument should be non-zero!"
  | d < 0 = error "no roots!"
  | otherwise = -b - sqrt (d) / (2*a)
  where d = b*b - 4*a*c

largerRoot :: Float -> Float -> Float -> Float
largerRoot a b c
  | a == 0 = error "the first argument should be non-zero!"
  | d < 0 = error "no roots!"
  | otherwise = -b + sqrt (d) / (2*a)
  where d = b*b - 4*a*c

-- 6
power2 :: Integer -> Integer
power2 0 = 1
power2 1 = 2
power2 n
  | n < 0 = 0
  | otherwise = 2 * power2 (n-1)

-- 7
mult :: Integer -> Integer -> Integer
mult m 0 = 0
mult 0 n = 0
mult m n
  | n < 0 = mult (-m) (-n)
  | otherwise = m + mult m (n-1)

-- 8
prod :: Integer -> Integer -> Integer
prod m n
  | m > n = error "the first argument should be smaller or equal to the second argument!"
  | m == n = m
  | otherwise = prod m (n-1) * n

fact :: Integer -> Integer
fact n
  | n==0 = 1
  | n > 0 = prod 1 n
  | otherwise = 0
