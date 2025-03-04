module Fichatp1PF where 

import Data.Char

perimetro :: Float -> Float
perimetro raio = 2 * pi * raio


distancia :: (Double,Double) -> (Double,Double) -> Double
distancia (x1,y1) (x2,y2)
    = sqrt ((x1-x2)^2+(y1-y2)^2)


primUlt lista = (head lista , last lista)


multiplo :: Int -> Int -> Bool
multiplo m n = if mod m n == 0 
            then True
            else False

multiplo2 m n = mod m n == 0


truncaImpar lista = if mod (length lista) 2 == 0 
                    then lista 
                    else tail lista 


max2 :: Int -> Int -> Int
max2 m n =
    if m > n then m
    else n

max3 :: Int -> Int -> Int -> Int
max3 a b c = if max2 a b == a
            then max2 a c
            else max2 b c


-- max3b a b c = max2 (max2 a b) c 

-- exercicio 2

-- 2.a
nRaizes :: Float -> Float -> Float -> Int 
nRaizes a b c
    | (b^2 - 4*a*c) == 0 = 1
    | (b^2 - 4*a*c) <  0 = 0
    | otherwise = 2

-- 2.b
raizes :: Float -> Float -> Float -> [Float]
raizes a b c    | nRaizes a b c == 2 = [(-b + raiz) / 2*a , (-b - raiz) / 2*a]
                | nRaizes a b c == 1 = [-b / 2*a]
                | otherwise = []

    where
        raiz = (sqrt (b^2 - 4*a*c))


type Hora = (Int,Int)

meiaNoiteEUmQuarto :: Hora
meiaNoiteEUmQuarto = (0,15)

duasMenosUmQuarto :: Hora
duasMenosUmQuarto = (13,45)

horaValida :: Hora -> Bool
horaValida (h,m) 
    = (h >= 0 && h < 24) && (m >= 0 && m < 60)


-- 3.b 
horaDepois :: Hora -> Hora -> Hora
horaDepois (x,y) (x1,y1)
                    | x > x1 || x == x1 && y > y1 = (x,y)
                    | otherwise = (x1,y1)


-- se hora1 > hora2 então True senão False

horaDepois2 :: Hora -> Hora -> Bool
horaDepois2 (h,m) (h1,m1) =
                        if h > h1 then True
                        else if h == h1 then m > m1
                        else False 
                        

-- 3.c 
conversor :: Hora -> Int
conversor (h,m) = (h * 60) + m 

-- 3.d
conversor2 :: Int -> Hora
conversor2 x = (div x 60 , mod x 60)

-- 3.e 
diferenca :: Hora -> Hora -> Int 
diferenca (h,m) (h1,m1) = abs (((h * 60) + m) - ((h1 * 60) + m1))

-- sem abs
diferenca2 :: Hora -> Hora -> Int 
diferenca2 (h,m) (h1,m1) =
    if (((h * 60) + m) - ((h1 * 60) + m1)) > 0 then (((h * 60) + m) - ((h1 * 60) + m1))
    else (-(((h * 60) + m) - ((h1 * 60) + m1)))


-- 3.f 
adHora1 :: Hora -> Int -> Hora
adHora1 (h,m) m1 = (h + div (m + m1) 60 , mod (m + m1) 60) 

-- 4
data HoraDT = H Int Int 
            deriving (Show , Eq)

meiaNoiteEVinte :: HoraDT
meiaNoiteEVinte = H 00 20

-- 4 - 3.a
horaValidaDT :: HoraDT -> Bool
horaValidaDT (H h m)
    = (h >= 0 && h < 24) && (m >= 0 && m < 60)


-- 5
data Semaforo = Verde | Amarelo | Vermelho 
            deriving (Show,Eq)

-- 5.1 
next :: Semaforo -> Semaforo 
next Verde    = Amarelo
next Amarelo  = Vermelho
next Vermelho = Verde 

-- 5.2 
stop :: Semaforo -> Bool
stop Vermelho = True
stop Verde = False 
stop Amarelo = False 

-- de outra forma
stop' Vermelho = True 
stop' _        = False               -- "_" significa "para todos os outros casos"

-- 5.3 
safe :: Semaforo -> Semaforo -> Bool
safe Vermelho Vermelho = True
safe Vermelho Amarelo  = True
safe Vermelho Verde    = True
safe Amarelo Vermelho  = True
safe Verde Vermelho    = True
safe _ _               = False 

-- outra forma mais simples
safe' :: Semaforo -> Semaforo -> Bool
safe' Vermelho _ = True
safe' _ Vermelho = True
safe' _ _        = False 

-- 6
data Ponto = Cartesiano Double Double | Polar Double Double
            deriving (Show,Eq)

-- 6.a TPC
posx :: Ponto -> Double
posx (Cartesiano x _) = abs x
posx (Polar y alpha) = abs (y * cos alpha)

-- 6.b 
posy :: Ponto -> Double
posy (Cartesiano _ y) = abs y
posy (Polar x alpha) = abs (x * sin alpha)

-- 6.c 
raio :: Ponto -> Double 
raio (Cartesiano x y) = sqrt (x^2 + y^2)
raio (Polar x1 alpha1) =  abs x1

-- 6.d (Acho que está mal) !!!
angulo :: Ponto -> Double
angulo (Cartesiano x y) = atan (y / x)
angulo (Polar x1 alpha) = alpha

-- 6.e 
dist :: Ponto -> Ponto -> Double
dist (Cartesiano x y) (Cartesiano x1 y1) = sqrt ((x1 - x)^2 + (y1 - y)^2)
dist (Polar r1 alpha1) (Polar r2 alpha2) = sqrt (r1^2 + r2^2 - (2 * r1 * r2) * cos (alpha2 - alpha1))

-- 7
data Figura = Circulo Ponto Double 
            | Retangulo Ponto Ponto 
            | Triangulo Ponto Ponto Ponto 
                deriving (Show,Eq)

-- 7.a
poligono :: Figura -> Bool 
poligono (Circulo _ r) = r > 0 
poligono (Retangulo (Cartesiano x y) (Cartesiano x1 y1)) = (x /= x1) && (y /= y1)
poligono (Triangulo (Cartesiano x y) (Cartesiano x1 y1) (Cartesiano x2 y2)) = (x /= x1) && (x /= x2) && (x1 /= x2) && (y /= y1) && (y /= y2) && (y1 /= y2)

-- 7.b
vertices :: Figura -> [Ponto]
vertices (Retangulo (Cartesiano x y) (Cartesiano x1 y1)) = [(Cartesiano x y) , (Cartesiano x y1) , (Cartesiano x1 y) , (Cartesiano x1 y1)]
vertices (Triangulo (Cartesiano x y) (Cartesiano x1 y1) (Cartesiano x2 y2)) = [(Cartesiano x y) , (Cartesiano x1 y1) , (Cartesiano x2 y2)]

-- 7.c
area :: Figura -> Double
area (Triangulo (Cartesiano x y) (Cartesiano x1 y1) (Cartesiano x2 y2)) =
    let a = dist (Cartesiano x y) (Cartesiano x1 y1)
        b = dist (Cartesiano x1 y1) (Cartesiano x2 y2)
        c = dist (Cartesiano x2 y2) (Cartesiano x y)
        s = (a+b+c) / 2 -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron

area (Retangulo (Cartesiano x y) (Cartesiano x1 y1)) = abs (x - x1) * abs (y - y1)

area (Circulo _ r) = r^2 * pi 

-- 7.d 
perimetro1 :: Figura -> Double 
perimetro1 (Circulo _ r) = r * 2 * pi 
perimetro1 (Retangulo (Cartesiano x y) (Cartesiano x1 y1)) = abs (x - x1) * 2 + abs (y - y1) * 2
perimetro1 (Triangulo (Cartesiano x y) (Cartesiano x1 y1) (Cartesiano x2 y2)) = (dist (Cartesiano x y) (Cartesiano x1 y1)) + (dist (Cartesiano x1 y1) (Cartesiano x2 y2) ) + (dist (Cartesiano x y) (Cartesiano x2 y2))

-- 8
isLower1 :: Char -> Bool
isLower1 x  
    | 97 <= ord x && ord x <= 122 = True
    |otherwise = False 

isDigit1 :: Char -> Bool
isDigit1 x = elem (ord x) [48..57]

isAlpha1 :: Char -> Bool
isAlpha1 x = isLower1 x || elem (ord x) [65..90]

toUpper1 :: Char -> Char
toUpper1 x = chr ( (ord x) - 32)

intToDigit1 :: Int -> Char
intToDigit1 x = chr (x + 48) 

digitToInt1 :: Char -> Int
digitToInt1 x = ord (x)-48