module Fichatp9PF where

import System.Random
import Data.Char

-- randomIO :: Random a => IO a 
-- randomRIO :: Random a => (a,a) IO a

data' :: IO (Int,Int,Int)
data' = do dia <- randomRIO (1,31)
           mes <- randomRIO (1,12)
           ano <- randomRIO (2000,2024)
           return (dia,mes,ano)

geraLista :: Int -> (Int,Int) -> IO [Int]
geraLista 0 _               = return []
geraLista n (minimo,maximo) = do valor  <- randomRIO (minimo,maximo) 
                                 valorn <- geraLista (n-1) (minimo,maximo)
                                 return (valor:valorn)


type Matriz a = [[a]]

geraMatriz :: (Int,Int) -> (Int,Int) -> IO (Matriz Int)
--          (linha,coluna)
geraMatriz (0,_) _ = return []
geraMatriz (_,0) _ = return []
geraMatriz (numeroLinhas,numeroColunas) (minimo,maximo) = do l  <- geraLista numeroColunas (minimo,maximo)
                                                             ls <- geraMatriz (numeroLinhas - 1 , numeroColunas) (minimo,maximo)
                                                             return (l:ls)
-- 1
-- 1. a 
bingo :: IO ()
bingo = do numeros <- geraNumeros []
           print numeros 

geraNumeros :: [Int] -> IO [Int]
geraNumeros numeros
    | length numeros == 90 = return numeros
    | otherwise = do 
        n <- randomRIO (1,90)
        print n 
        let nl = if elem n numeros
                 then numeros 
                 else n:numeros
        geraNumeros nl 

{- bingo' :: IO ()
bingo' = do l <- geraIndices [1..90] []
            print l 

geraIndices :: [Int] -> Int -> IO ([Int],[Int])
geraIndices [x] l = return (x:l,[])
geraIndices s   l = do i <- randomRIO (0,length s - 1)
                        -- remover em s o valor v da pos i 
                        -- e inserir v em l -}


{-
import System.Random
import System.Random.Shuffle (shuffleM)

bingo :: IO ()
bingo = do
    numeros <- geraNumeros
    print numeros

geraNumeros :: IO [Int]
geraNumeros = shuffleM [1..90] -- Gera e embaralha os números de 1 a 90
-}

-- 1.b -- resolução errada, tem um erro no 'if' (a ser corrigido)
{-
mastermind :: IO ()
mastermind = do chaveSecreta <- gerarChaveSecreta
                putStrLn "Insira 4 digitos"
                jogarMM cs 
                putStrLn "Adivinhou!"

gerarChaveSecreta :: IO (Int,Int,Int,Int)
gerarChaveSecreta = do 
                       a <- randomRIO (0,9)
                       b <- randomRIO (0,9)
                       c <- randomRIO (0,9)
                       d <- randomRIO (0,9) 
                       return (a,b,c,d)

jogarMM :: (Int,Int,Int,Int) -> IO ()
jogarMM chaveSecreta = do adivinhaJogador <- getLine
             let quantos = quantosIguais chaveSecreta adivinhaJogador
             if (quantos == 4) then return ()
             else do putStrLn "Tem " ++ (show quantos) ++ " certos!"
                  jogarMM chaveSecreta

-- quantos certos na posição certa
quantosIguais :: (Int,Int,Int,Int) -> String -> Int 
quantosIguais (x,y,z,w) (a:b:c:d:_) =
    length $ filter (== True) 
    [ x = digitToInt a ,
      y == digitToInt b ,
      z == digitToInt c ,
      w == digitToInt d
    ]
-}