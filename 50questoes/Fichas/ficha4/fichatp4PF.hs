module Fichatp4PF where
import Data.Char

-- 1
digitAlpha :: String -> (String,String)
digitAlpha [] = ("","")
digitAlpha (h:t) 
    | isDigit h = (ll , h:ld)
    | isAlpha h = (h:ll , ld)
    | otherwise = (ll,ld)
    where (ll,ld) = digitAlpha t

-- 2
nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (x:xs)
    | x < 0 = (nn + 1 , nz , np )
    | x == 0 = (nn , nz + 1 , np)
    | x > 0 = (nn , nz , np + 1)
    where (nn,nz,np) = nzp xs

-- 3 
divMod' :: Integral a => a -> a -> (a, a)
divMod' 0 _ = (0,0)
divMod' m n
    | m < n = (0,m)
    | otherwise = (1+q , r)
    where (q,r) = divMod' (m-n) n 

myDiv :: Int -> Int -> Int
myDiv m n 
    | m < n = 0 
    | otherwise = 1 + myDiv (m-n) n

myMod :: Int -> Int -> Int 
myMod m n 
    | m < n = m 
    | otherwise = myMod (m-n) n

-- 4 
fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits l = aux 0 l 
    where aux n [] = n
          aux n (x:xs) = aux (x + n*10) xs 

-- 5 
maxSumInt :: (Num a , Ord a) => [a] -> a 
maxSumInt l = aux 0 0 l 
    where 
      aux m _ [] = m 
      aux m s (x:xs)
            | s + x > m = aux (s + x) (s + x) xs 
            | otherwise = aux m (s + x) xs

-- 6 
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = aux 0 1 n 
    where aux n1 n0 0 = n1 
          aux n1 n0 n = aux (n1+n0) n1 (n-1)


-- 7 
-- 6 e 7 -> TPC !!! 

-- 9

-- a) [2^x | x <- [0..10]]

-- b) [(x,y) | x <- [1..5], y <- [5,4..1], x+y == 6]
--    ou
--    [(x,6-x) | x <- [1..5]]

-- c) [[x | x <- [1..y]] |y <- [1..5]]
--    ou
--    [take y [1..] |y <- [1..5]]

-- d)  [take y [1,1..] |y <- [1..5]]
--     ou 
--     [[1^x] | x <- [1..y] | y <- [1..5]]

-- e) [product [x | x <- [1..y]] | y <- [1..6]]