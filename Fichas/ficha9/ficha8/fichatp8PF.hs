module Fichatp8PF where
import Data.List
import Data.Char (toUpper)

-- 1. 
data Frac = F Integer Integer

-- 1.a 
normaliza :: Frac -> Frac
normaliza (F x y)
    | y < 0 = F (div (-x) m) (div y m)
    | otherwise = F (div x m) (div y m)
        where m = mdc x y 

mdc x 0 = x 
mdc 0 y = y 
mdc x y = mdc y (mod x y)

-- 1.b 
instance Eq Frac where
    (==) = eqFrac 

eqFrac :: Frac -> Frac -> Bool
eqFrac f1 f2 = x1 == x2 && y1 == y2
    where
        F x1 y1 = normaliza f1 
        F x2 y2 = normaliza f2 

-- 1.c 
instance Ord Frac where 
    (<=) = ordFrac

ordFrac f1 f2 = x1*y2 <= x2*y1
    where 
        (F x1 y1) = normaliza f1 
        (F x2 y2 )= normaliza f2

-- 1.d 
instance Show Frac where 
    show = showFrac 

showFrac :: Frac -> [Char]
showFrac f = show x ++ "/" ++ show y
    where F x y = normaliza f

-- 1.e 
instance Num Frac where 
    (+) = somaFrac 
    (*) = multFrac 
    abs = absFrac 
    negate = negateFrac
    signum = signumFrac 
    fromInteger = fromIntegerFrac 

somaFrac :: Frac -> Frac -> Frac
somaFrac f1 f2 = normaliza (F (x1*y2 + x2*y1) (y1*y2))
    where
        (F x1 y1) = normaliza f1 
        (F x2 y2) = normaliza f2 

multFrac :: Frac -> Frac -> Frac
multFrac f1 f2 = normaliza (F (x1*x2) (y1*y2))
    where
        (F x1 y1) = normaliza f1 
        (F x2 y2) = normaliza f2 

absFrac :: Frac -> Frac
absFrac f1 = normaliza (F (abs x) (abs y))
    where (F x y) = normaliza f1 

negateFrac :: Frac -> Frac
negateFrac f1 = F (negate x) y
    where (F x y) = normaliza f1 

signumFrac :: Frac -> Frac
signumFrac f = normaliza $ F (signum x) (signum y)
    where (F x y) = normaliza f 

fromIntegerFrac :: Integer -> Frac
fromIntegerFrac x = F x 1

-- 2. 
data Exp a = Const a
             | Simetrico (Exp a)
             | Mais (Exp a) (Exp a)
             | Menos (Exp a) (Exp a)
             | Mult (Exp a) (Exp a)

e1 :: Exp Int 
e1 = Mult (Mais (Const 4) (Const 3))
         (Mult (Const 1) (Const 2))

e2 :: Exp Int
e2 = Mult (Mais (Const 3) (Const 4))
         (Mult (Const 2) (Const 1))

-- 2.a 
instance Show a => Show (Exp a) where
    show = showExp 

showExp :: Show a => Exp a -> String 
showExp (Const a) = show a 
showExp (Simetrico a) = "(" ++ "-" ++ showExp a ++ ")"
showExp (Mais a b) = "(" ++ showExp a ++ " + " ++ showExp b ++ ")"
showExp (Menos a b) = "(" ++ showExp a ++ " - " ++ showExp b ++ ")"
showExp (Mult a b) = "(" ++ showExp a ++ " * " ++ showExp b ++ ")"

-- 2.b
{-
instance Eq a => Eq (Exp a) where 
     (==) = eqExp

eqExp :: Eq a => Exp a -> Exp a -> Bool               -- esta definição apenas vê se ambas são totalmente iguais, não olhando sequer para o resultado
eqExp (Const a) (Const b) = a == b 
eqExp (Simetrico a) (Simetrico b) = a == b 
eqExp (Mais a1 a2) (Mais b1 b2) = eqExp a1 b1 && eqExp a2 b2 
eqExp (Menos a1 a2) (Menos b1 b2) = eqExp a1 b1 && eqExp a2 b2 
eqExp (Mult a1 a2) (Mult b1 b2) = eqExp a1 b1 && eqExp a2 b2 
-}

instance (Eq a , Num a) => Eq (Exp a) where 
    (==) = eqExp1

eqExp1 :: (Eq a , Num a) => Exp a -> Exp a -> Bool               -- definição correta
eqExp1 exp1 exp2 = calcula exp1 == calcula exp2


calcula :: Num a => Exp a -> a
calcula (Const constante) = constante
calcula (Simetrico e) = - calcula e
calcula (Mais e1 e2) = calcula e1 + calcula e2
calcula (Menos e1 e2) = calcula e1 - calcula e2
calcula (Mult e1 e2) = calcula e1 * calcula e2

-- 2.c 
somaExp :: Num a => Exp a -> Exp a -> Exp a 
somaExp e1 e2 = Const (calcula e1 + calcula e2)

subExp :: Num a => Exp a -> Exp a -> Exp a 
subExp e1 e2 = Const (calcula e1 - calcula e2)

multExp :: Num a => Exp a -> Exp a -> Exp a 
multExp e1 e2 = Const (calcula e1 * calcula e2)

absExp :: Num a => Exp a -> Exp a 
absExp e = Const (abs (calcula e))

signumExp :: Num a => Exp a -> Exp a                   
signumExp e = Const $ signum (calcula e)

fromIntegerExp :: Num a => Integer -> Exp a 
fromIntegerExp i = Const (fromInteger i)

instance Num a => Num (Exp a) where 
    (+) = somaExp
    (-) = subExp 
    (*) = multExp
    abs = absExp
    signum = signumExp
    fromInteger = fromIntegerExp

menorIgual :: (Num a , Ord a) => Exp a -> Exp a -> Bool 
menorIgual e1 e2 = calcula e1 <= calcula e2 

instance (Num a , Eq a , Ord a) => Ord (Exp a) where 
    (<=) = menorIgual

-- 3 
data Movimento = Credito Float | Debito Float
data Data = D Int Int Int
data Extrato = Ext Float [(Data, String, Movimento)]

ext :: Extrato 
ext = Ext 500 [(D 4 5 2001,"Deposito",Credito 300),
               (D 5 9 2003,"Propinas",Debito 100),
               (D 27 11 1999,"Financas",Debito 150),
               (D 13 3 2002,"Emprego",Credito 200)]

-- 3.a 
instance Eq Data where 
    (==) = eqData 

eqData (D d1 m1 a1) (D d2 m2 a2) = (a2 == a1) && (m2 == m1) && (d2 == d1)

instance Ord Data where 
    (<=) = ordData 

ordData :: Data -> Data -> Bool
ordData (D d1 m1 a1) (D d2 m2 a2) = a2 > a1 || (a2 == a1) && m2 > m1 || (a2 == a1) && (m2 == m1) && (d2 >= d1)

-- 3.b 
instance Show Data where 
    show = showData 

showData (D dia mes ano) = show dia ++ "/" ++ show mes ++ "/" ++ show ano 

-- 3.c 
ordena :: Extrato -> Extrato
ordena (Ext saldo movimentos) = Ext saldo (sortOn fstMovimentos movimentos)
    where 
        fstMovimentos (x,y,z) = x

-- 3.d 
instance Show Extrato where 
    show = showExtrato 

showExtrato extrato = let (Ext saldo movimentos) = ordena extrato
    in "-----------------------------------------\n" ++
       "Saldo Inicial: " ++ show saldo ++ "\n" ++
       "-----------------------------------------\n" ++ 
       "Data        Descrição    Crédito   Débito\n" ++
       "-----------------------------------------" ++ "\n" ++ 
       concat (map showMovs movimentos) ++ 
       "-----------------------------------------" ++ "\n" ++ 
       "Saldo Atual: " ++ show (saldoAtual extrato) ++ "\n" ++ 
       "-----------------------------------------"



showMovs :: (Data,String,Movimento) -> String 
showMovs (date,descricao,Credito x) = 
    show date ++ 
    (replicate (data_max - length (show date)) ' ') ++ 
    (map toUpper descricao) ++ 
    (replicate (desc_max - length descricao) ' ') ++ 
    show x ++ "\n"
    where data_max = 12
          desc_max = 13
showMovs (date,descricao,Debito x) = 
    show date ++ 
    (replicate (data_max - length (show date)) ' ') ++ 
    (map toUpper descricao) ++ 
    (replicate (desc_max - length descricao) ' ') ++ 
    replicate cred_max ' ' ++ 
    show x ++ "\n"
    where data_max = 12
          desc_max = 12
          cred_max = 11

saldoAtual :: Extrato -> Float
saldoAtual (Ext ini list) = ini + sum listMovs 
    where listMovs = map (\ (_,_,mov) -> case mov of 
                                              Credito x -> x
                                              Debito y -> (-y)) list
