module Fichatp5PF where

import Data.List

-- 1
--a)
any' :: (a -> Bool) -> [a] -> Bool
any' p []       = False 
any' p (x:xs) 
    | p x       = True
    | otherwise = any' p xs 

-- resolução do professor
any'' :: (a -> Bool) -> [a] -> Bool
any'' p []     = False 
any'' p (x:xs) = p x || any'' p xs

-- b)
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 
zipWith' f _ _           = []

-- d)
dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs)
    | p x       = dropWhile' p xs 
    | otherwise = x : xs 

-- e)
span' :: (a-> Bool) -> [a] -> ([a],[a])
span' p l = (takeWhile p l , dropWhile' p l)

-- resolução mais eficiente
spanE :: (a-> Bool) -> [a] -> ([a],[a])
spanE p []          = ([],[])
spanE p (x:xs)
    | p x           = (x : zz, rr)
    | otherwise     = ([] , x : xs)

        where 
            (zz,rr) = spanE p xs

-- f)
deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' _ _ [] = []
deleteBy' p x (y:ys) 
    | p x y      = ys 
    | otherwise  = y : deleteBy' p x ys 

r                = deleteBy' (\x y -> snd x == snd y) (1,2) [(3,3),(2,2),(4,2)]



insereOrd :: Ord b => (a -> b) -> a -> [a] -> [a]
insereOrd _ e []    = [e]
insereOrd f e (h:t)
    | f e < f h     = e : h : t
    | otherwise     = h : insereOrd f e t 

sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f (h:t)     = insereOrd f h (sortOn' f t)

r'                  = sortOn' fst [(3,1),(1,2),(2,5)] -- == [(1,2),(2,5),(3,1)]

-- 2 
type Polinomio = [Monomio]
type Monomio = (Float,Int)

p1 :: Polinomio
p1 = [(2,3) , (3,4) , (5,3) , (4,5)]

p2 :: Polinomio
p2 = [(3,2) , (6,1) , (2,0)]

-- a) 
selgrau :: Int -> Polinomio -> Polinomio 
selgrau _ [] = []
selgrau expoente ((numero,exp):ns)
    | exp == expoente = (numero , exp) : selgrau expoente ns
    | otherwise = selgrau expoente ns 

selgrauFilter :: Int -> Polinomio -> Polinomio
selgrauFilter indice polinomio = filter (\ (coeficiente,expoente) -> expoente == indice) polinomio
 
-- b)
conta :: Int -> Polinomio -> Int 
conta _ [] = 0
conta expoente ((numero,exp):ns)
    | exp == expoente = 1 + conta expoente ns
    | otherwise = conta expoente ns 

contaFilter :: Int -> Polinomio -> Int
contaFilter indice polinomio = length $ filter (\ (coeficiente,expoente) -> expoente == indice) polinomio

contaFoldr :: Int -> Polinomio -> Int 
contaFoldr indice polinomio = foldr (\(numero,expoente) acc -> if expoente == indice then acc+1 else acc) 0 polinomio

-- c) 
grauMap :: Polinomio -> Int 
grauMap polinomio = maximum (map snd polinomio)

grauFoldr :: Polinomio -> Int
grauFoldr polinomio | not (null polinomio) = foldr (\ (numero,expoente) acc -> if expoente > acc then expoente else acc) 0 polinomio

-- d) 
deriv :: Polinomio -> Polinomio 
deriv [] = []
deriv ((numero, exp):ns) = (fromIntegral exp * numero, exp - 1) : deriv ns

derivMapFilter :: Polinomio -> Polinomio 
derivMapFilter polinomio = map (\(x,y) -> (x * fromIntegral y , y-1)) (filter (\ (x,y) -> y /= 0) polinomio)

derivFoldr polinomio = foldr f [] polinomio
    where f (coeficiente,0) acc = acc 
          f (coeficiente,expoente) acc = (coeficiente * fromInteger expoente , expoente -1) : acc

-- e) 
calculaMap :: Float -> Polinomio -> Float
calculaMap x polinomio = sum (map (\(numero,expoente) -> numero * x^expoente) polinomio)

calculaFoldr x polinomio = foldr (\(numero,expoente) acc -> numero * expoente^x + acc ) 0 polinomio 

-- 3
type Mat a = [[a]]

matriz :: Mat Int
matriz = [[1,2,3],
          [0,4,5], 
          [0,0,6]
         ]

-- a)
dimOK :: Mat a -> Bool
dimOK [a]                    = True 
dimOK (m1:m2:ms)
    | length m1 == length m2 = dimOK (m2:ms)
    | otherwise              = False 

-- com funções de ordem superior
dimOK' m = length (nub (map length m)) == 1

-- b)
dimMat :: Mat a -> (Int,Int)
dimMat ((h:t):ms) = (length ((h:t):ms) , length (h:t))

-- c)
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat (l1:ls1) (l2:ls2) = zipWith (+) l1 l2 : addMat ls1 ls2 