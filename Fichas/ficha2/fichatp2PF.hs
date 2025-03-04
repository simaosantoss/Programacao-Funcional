module Fichatp2PF where
import qualified Data.Char as Data
import Data.Char (isDigit)
import Data.Char (isLower)
import Data.Char (isDigit, digitToInt)

-- a
funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys)

{-
funA [2,3,5,1] = 2^2 + funA[3,5,1]
                = 2^2 + 3^2 + funA[5,1]
                = 2^2 + 3^2 + 5^2 + funA[1]
                = 2^2 + 3^2 + 5^2 + 1^2 + funA[]
                = 2^2 + 3^2 + 5^2 + 1^2 + 0
                = 39 
-}

-- b
funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2)==0 then h : (funB t)
             else (funB t)

-- c
funC (x:y:t) = funC t
funC [x] = [x]
funC [] = []

-- d
funD l = g [] l
g acc [] = acc
g acc (h:t) = g (h:acc) t


-- 2
-- a)
dobros :: [Float] -> [Float] 
dobros [] = []
dobros (x:xs) = 2*x : dobros xs

-- b)
numOcorre :: Char -> String -> Int
numOcorre c [] = 0
numOcorre c (h:t)
    |c == h = 1 + numOcorre c t 
    |otherwise = numOcorre c t 

-- c)
positivos :: [Int] -> Bool
positivos [] = True 
positivos (h:t)
    |h>=0 = positivos t
    |otherwise = False 

-- d)
soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) 
    |h<=0 = soPos t
    |otherwise = h : soPos t

-- e)
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t)
    |h<0 = h + somaNeg t 
    |otherwise = somaNeg t 

-- f) 
tresUlt :: [a] -> [a]
tresUlt l 
    |length l <= 3 = l 
    |otherwise = tresUlt (tail l)

-- g)
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos (h:t) = snd h : segundos t

segundos' :: [(a,b)] -> [b]
segundos' [] = []
segundos' ((x,y):t) = y : segundos t

-- h)
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros a [] = False 
nosPrimeiros a ((x,y):t)
    |a == x = True
    |a /= x = nosPrimeiros a t

nosPrimeiros' :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros' a [] = False 
nosPrimeiros' a ((x,y):t) = a == x || (nosPrimeiros a t)

-- i)
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((x,y,z):t) = let (sx,sy,sz) = sumTriplos t
                       in (sx+x , sy+y , sz+z)

-- 3
-- a) 
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) 
    | isDigit h = h : soDigitos t
    | otherwise = soDigitos t

-- b)
minusculas :: [Char] -> Int 
minusculas [] = 0 
minusculas (h:t)
    | isLower h = 1 + minusculas t
    | otherwise = minusculas t

-- c)
nums :: String -> [Int]
nums [] = []
nums (h:t)
    | isDigit h = digitToInt h : nums t
    | otherwise = nums t

-- 4
type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- a)
conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n ((c,e):ce)
    | n == e = 1 + conta n ce
    | otherwise = conta n ce

-- b)
grau :: Polinomio -> Int
grau [] = 0
grau [(_,e)] = e
grau ((_,e):ce) = max e (grau ce)

-- c)
selgrau :: Int -> Polinomio -> Polinomio
selgrau n [] = []
selgrau n ((c,e):ce) 
    | n == e = (c,e) : selgrau n ce
    | otherwise = selgrau n ce 

-- d)
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((c,e):ce)
    | e > 0 = (c * fromIntegral e, e - 1) : deriv ce
    | otherwise    = deriv ce

-- e)
calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula x ((coef,exp):ce) = coef * (x^exp) + calcula x ce

-- f)
simp :: Polinomio -> Polinomio
simp [] = []
simp ((coef, exp):ce)
    | coef == 0 = simp ce
    | otherwise = (coef, exp) : simp ce

-- g)
mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = [] 
mult (coef1, exp1) ((coef2, exp2):ce) = (coef1*coef2 , exp1+exp2) : mult (coef1,exp1) ce

-- h) DUVIDAS
adicionaMonomio :: Monomio -> Polinomio -> Polinomio
adicionaMonomio (coef1,exp2) [] = [(coef1,exp2)]
adicionaMonomio (coef1,exp1) ((coef2,exp2):ce) 
    | exp1 == exp2 = (coef1 + coef2,exp1) : ce
    | otherwise = (coef2,exp2) : adicionaMonomio (coef1,exp1) ce

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza (x:xs) = adicionaMonomio x (normaliza xs)

-- i)
soma :: Polinomio -> Polinomio -> Polinomio
soma polinomio1 polinomio2 = normaliza (polinomio1 ++ polinomio2)

-- j)
