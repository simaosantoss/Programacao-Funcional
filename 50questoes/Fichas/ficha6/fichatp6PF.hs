module Fichatp6PF where

data BTree a = Empty
               | Node a (BTree a) (BTree a)
            deriving (Show, Eq)
        
--1.a 
altura :: BTree a -> Int
altura Empty = 0
altura (Node _ left right) = 1 + max (altura left) (altura right)

-- 1.b
contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node _ left right) = 1 + contaNodos left + contaNodos right

-- 1.c
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node _ left right) = folhas left + folhas right 

-- 1.d 
prune :: Int -> BTree a -> BTree a
prune  _ Empty = Empty 
prune 0 _ = Empty 
prune n (Node x left right) = Node x (prune (n-1) left) (prune (n-1) right)

-- Podar uma arvore a nivel i tem que ter altura 1 
propriedadePrune i t = altura (prune i t)  == i 

-- 1.e 
path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node a left right) = [a]
path (h:t) (Node a left right)
    | h == False = a : path t left 
    | h == True = a : path t right 

-- 1.f 
mirror :: BTree a -> BTree a
mirror Empty = Empty 
mirror (Node a left right) = Node a (mirror right) (mirror left)

-- 1.g 
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT _ Empty _ = Empty
zipWithBT _ _ Empty = Empty
zipWithBT f (Node x left1 right1) (Node y left2 right2) =
    Node (f x y) (zipWithBT f left1 left2) (zipWithBT f right1 right2)

-- 1.h 
unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty , Empty , Empty)
unzipBT (Node (x,y,z) left right) = (Node x left1 right1 , Node y left2 right2 , Node z left3 right3)
    where 
        (left1 , left2 , left3) = unzipBT left 
        (right1 , right2 , right3) = unzipBT right



---------------------------------
-- 2 -- árvore binária de procura
---------------------------------

abp :: BTree Int                                -- para realizar testes
abp = Node 4
        (Node 2 ( Node 1 Empty Empty) 
                ( Node 3 Empty Empty))
        (Node 8 ( Node 7 Empty Empty)
                ( Node 15 ( Node 10 Empty Empty)
                          Empty) )

--      4
--     /  \
--    2    8
--   / \   / \
--  1   3 7  15
--          /  
--         10 

-- 2.a 
minimo :: Ord a => BTree a -> a
minimo (Node i Empty _) = i 
minimo (Node _ left _) = minimo left 

-- 2.b 
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node i Empty right) = right
semMinimo (Node i left right) = Node i (semMinimo left) right -- desce-se até ao menor para o retirar

-- 2.c 
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node i Empty right) = (i , right)
minSmin a = (minimo a , semMinimo a)

-- 2.d 
remove :: Ord a => a -> BTree a -> BTree a
remove a Empty = Empty
remove elemento (Node i left right)
    | elemento < i = Node i (remove elemento left) right 
    | elemento > i = Node i left (remove elemento right)
    | elemento == i = case (left , right) of
                                            (Empty , _)    -> right 
                                            (_ , Empty) -> left 
                                            _              -> let m = minimo right 
                                                                  right' = semMinimo right
                                                               in Node m left right'

-- 3
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL 
            deriving (Show,Eq)
data Classificacao = Aprov Int
                    | Rep
                    | Faltou
                    deriving (Show,Eq)
type Turma = BTree Aluno --  árvore binária de procura (ordenada por número)

-- 3.a 
inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False 
inscNum numero (Node (numeroProcura,_,_,_) left right)
    | numero == numeroProcura = True 
    | numero < numeroProcura = inscNum numero left 
    | numero > numeroProcura = inscNum numero right 

-- 3.b 
inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False 
inscNome nome (Node (_,nomeProcura,_,_) left right)
    | nome == nomeProcura = True 
    | otherwise = inscNome nome left || inscNome nome right 

-- 3.c 
trabEst :: Turma -> [(Numero, Nome)]
trabEst Empty = []
trabEst (Node (num, nome, regime, _) left right)
  | regime == TE = trabEst left ++ [(num, nome)] ++ trabEst right
  | otherwise = trabEst left ++ trabEst right 

-- 3.d 
nota :: Numero -> Turma -> Maybe Classificacao
nota _ Empty = Nothing 
nota numero (Node (numeroProcura,_,_,classificacao) left right)
    | numero == numeroProcura = Just classificacao
    | numero < numeroProcura  = nota numero left
    | numero > numeroProcura  = nota numero right

-- 3.e 
percFaltas :: Turma -> Float
percFaltas turma = 
  let (faltas, total) = contaFaltasETotal turma
  in if total == 0 then 0.0 else (fromIntegral faltas / fromIntegral total) * 100

-- Função auxiliar para contar faltas e total de alunos
contaFaltasETotal :: Turma -> (Int, Int)
contaFaltasETotal Empty = (0, 0)
contaFaltasETotal (Node (_, _, _, classificacao) left right) =
  let (faltasL, totalL) = contaFaltasETotal left
      (faltasR, totalR) = contaFaltasETotal right
      faltaAtual = if classificacao == Faltou then 1 else 0
  in (faltasL + faltasR + faltaAtual, totalL + totalR + 1)

-- 3.f 
mediaAprov :: Turma -> Float
mediaAprov turma =
  let (soma, total) = somaEAprovados turma
  in if total == 0 then 0.0 else fromIntegral soma / fromIntegral total

-- Função auxiliar para somar notas de aprovados e contar aprovados
somaEAprovados :: Turma -> (Int, Int)
somaEAprovados Empty = (0, 0)
somaEAprovados (Node (_, _, _, classificacao) left right) =
  let (somaL, totalL) = somaEAprovados left
      (somaR, totalR) = somaEAprovados right
      (notaAtual, aprovAtual) = case classificacao of
        Aprov n -> (n, 1)
        _       -> (0, 0)
  in (somaL + somaR + notaAtual, totalL + totalR + aprovAtual)

-- 3.g
aprovAv :: Turma -> Float
aprovAv turma =
  let (aprovados, avaliados) = contaAprovadosEAvaliados turma
  in if avaliados == 0 then 0.0 else fromIntegral aprovados / fromIntegral avaliados

-- Função auxiliar para contar aprovados e avaliados
contaAprovadosEAvaliados :: Turma -> (Int, Int)
contaAprovadosEAvaliados Empty = (0, 0)
contaAprovadosEAvaliados (Node (_, _, _, classificacao) left right) =
  let (aprovL, avalL) = contaAprovadosEAvaliados left
      (aprovR, avalR) = contaAprovadosEAvaliados right
      (aprovAtual, avalAtual) = case classificacao of
        Aprov _ -> (1, 1)  -- Se aprovado, conta como 1 aprovado e 1 avaliado
        Rep     -> (0, 1)  -- Se reprovado, conta como 0 aprovados e 1 avaliado
        Faltou  -> (0, 0)  -- Se faltou, não conta como avaliado
  in (aprovL + aprovR + aprovAtual, avalL + avalR + avalAtual)