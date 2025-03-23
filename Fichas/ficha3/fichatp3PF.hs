module Fichatp3PF where
import GHC.Base (RuntimeRep(TupleRep))

data Hora = H Int Int
            deriving Show

type Etapa = (Hora,Hora)

type Viagem = [Etapa]

v :: [Etapa]
v = [(H 9 30, H 10 25), (H 11 20, H 12 45), (H 13 30, H 14 45)]

horaValida :: Hora -> Bool
horaValida (H h m)
    = (h >= 0 && h < 24) && (m >= 0 && m < 60)

maiorHora :: Hora -> Hora -> Bool
maiorHora (H h1 m1) (H h2 m2)
    | h1 > h2 = True 
    | h1 == h2 && m1 > m2 = True
    | otherwise = False 


-- 1

-- a)
etapaValida :: Etapa -> Bool 
etapaValida (hi,hf) =
            horaValida hi &&
            horaValida hf &&
            maiorHora hf hi 

-- b) 
{- [e] lista singular || (e:[]) lista singular -}
viagemValida :: Viagem -> Bool
viagemValida [e] = etapaValida e
viagemValida (e1:e2:vs)
    | etapaValida e1 && etapaValida e2 && maiorHora (fst e2) (snd e1)
        = viagemValida (e2:vs)
    | otherwise = False

-- c)
horaPartidaChegada :: Viagem -> (Hora,Hora)
horaPartidaChegada viagem = (fst (head viagem) , snd (last viagem))

-- d)
tempoTotalAViajar :: Viagem -> Int 
tempoTotalAViajar [] = 0
tempoTotalAViajar ((hi,hf):vs) = 
        (horasMinutos hf - horasMinutos hi) + tempoTotalAViajar vs
horasMinutos (H h m) = h*60 + m

-- e) 
tempoDeEspera :: Viagem -> Int 
tempoDeEspera [e] = 0
tempoDeEspera ((hi1,hf1):(hi2,hf2):vs) = 
        (horasMinutos hi2 - horasMinutos hf1) + tempoDeEspera ((hi2,hf2):vs)

-- resolução do professor (tempo total - tempo a viajar)

{- tempoDeEspera' :: Viagem -> Bool 
tempoDeEspera' = tt - tempoTotalAViajar v
    where (hp,hc) = horaPartidaChegada v
           tt    = horasMinutos hc - horasMinutos hp -}

-- 2 --------------------------------------------------------------------------
type Ponto = (Double,Double)
type Poligonal = [Ponto]

distancia :: Ponto -> Ponto -> Double
distancia (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- a)
comprimento :: Poligonal -> Double 
comprimento [] = 0
comprimento [_] = 0
comprimento (p1:p2:ps) = distancia p1 p2 + comprimento (p2:ps)

-- b)
fechada :: Poligonal -> Bool
fechada [] = False
fechada [_] = False
fechada ps = head ps == last ps

{-
fechada´ :: Poligonal -> Bool
fechada´ [] = False
fechada´ [_] = False
fechada´ (p1:p2:ps)
    | p1 == p2 = True
    | p1 `elem` (p2:ps) = True
    | otherwise = fechada´ (p2:ps)
-}

-- c)
data Figura = Circulo Ponto Double 
            | Retangulo Ponto Ponto 
            | Triangulo Ponto Ponto Ponto 
                deriving (Show,Eq)

-- 3
data Contacto = Casa Integer
            | Trab Integer
            | Tlm Integer
            | Email String
                deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]

-- para testar
agendaInicial :: Agenda
agendaInicial = [("Joao", [Casa 123456789, Tlm 987654321]),
                 ("Maria", [Trab 1122334455, Email "maria@exemplo.com"])]
-- a)
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email [] = [(nome, [Email email])]
acrescEmail nome email ((n, contactos) : resto)
    | nome == n = (nome , Email email : contactos) : resto 
    | otherwise = (n , contactos) : acrescEmail nome email resto

-- b)
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails nome [] = Nothing
verEmails nome ((nomeProcura,contactoProcura):resto)
    | nome == nomeProcura = Just [email | Email email <- contactoProcura]
    | otherwise = verEmails nome resto 





-- 4
type Dia = Int
type Mes = Int
type Ano = Int
type Nome2 = String
data Data = D Dia Mes Ano
            deriving (Show, Eq)
type TabDN = [(Nome2,Data)]

aniv :: TabDN 
aniv = [ ("Ana" , D 1 1 2000)
        , ("Rui" , D 2 2 2010)
        , ("Ze" , D 3 3 2020)]

procura :: Nome2 -> TabDN -> Maybe Data 
procura n [] = Nothing
procura n ((na,dna):ls) 
        | n == na   = Just dna
        | otherwise = procura n ls

-- b)
{-
idade :: Data -> Nome2 -> TabDN -> Maybe Int
idade dt n tab
    | procura n tab == Nothing = Nothing 
    | otherwise = 
                    let (Just dn) = procura n tab
                    in Just (subIdades dt dn)
-}
idade :: Data -> Nome2 -> TabDN -> Maybe Int
idade dt n tab = case procura n tab of 
    Nothing   -> Nothing 
    (Just dn) -> Just (subIdades dt dn)

subIdades (D d1 m1 a1) (D d2 m2 a2) = abs (a2 - a1)

-- c)
anterior :: Data -> Data -> Bool                          -- True quando a primeira data é anterior à segunda
anterior (D d1 m1 a1) (D d2 m2 a2)                        -- False quando a primeira é posterior à segunda
    | a2 == a1 && m2 == m1 && a2 > a1 = True
    | a2 == a1 && m2 > m1 = True 
    | a2 > a1 = True
    | otherwise = False

-- d)
insereDt :: (Nome,Data) -> TabDN -> TabDN
insereDt (n,dn) [] = [(n,dn)]
insereDt (n,dn) ((nome,dt):t)
    | anterior dn dt = (n,dn):((nome,dt):t)
    | otherwise = (nome,dt) : insereDt (n,dn) t

ordena :: TabDN -> TabDN
ordena [] = []
ordena (h:t) = insereDt h (ordena t)

-- 5
-- a)
data Movimento = Credito Float | Debito Float
                 deriving Show
-- data Data = D Int Int Int
--            deriving Show
data Extrato = Ext Float [(Data, String, Movimento)]
                deriving Show

outubro = Ext 1000.5
          [ (D 2 10 2024 , "ATM" , Debito 20)
          , (D 5 10 2024 , "EDP" , Debito 44.6)
          , (D 6 10 2024 , "Salario" , Credito 850)
          ]

-- a)
extValor :: Extrato -> Float -> [Movimento]                                
extValor (Ext v []) m = []
extValor (Ext v mvs) m = movimentosMaior mvs m

movimentosMaior :: [(Data,String,Movimento)] -> Float -> [Movimento]                          
movimentosMaior [] v = []
movimentosMaior ((_ ,_,(Credito x)):t) v | x >= v = Credito x : movimentosMaior t v
                                         | otherwise = movimentosMaior t v
movimentosMaior ((_ ,_,(Debito x)):t) v  | x >= v = Debito x : movimentosMaior t v
                                         | otherwise = movimentosMaior t v

-- b)
filtro :: Extrato -> [String] -> [(Data,Movimento)]                                           
filtro (Ext _ mvs) descs = filtroMovimentos mvs descs 
    where filtroMovimentos :: [(Data,String,Movimento)] 
                                -> [String] -> [(Data,Movimento)]
          filtroMovimentos [] _ = []
          filtroMovimentos ((d,desc,m):t) descs 
            | desc `elem` descs = (d,m) : filtroMovimentos t descs 
            | otherwise = filtroMovimentos t descs

-- c)
creDeb :: Extrato -> (Float,Float)
creDeb (Ext _ []) = (0,0)
creDeb (Ext _ (_,_,m):resto)
    | isCre m = somaDuplo (valorMovimento m , 0) (creDeb (Ext _ (resto)))
    | otherwise = somaDuplo (0 , valorMovimento m) (creDeb (Ext _ (resto)))

somaDuplo :: (Float,Float) -> (Float,Float) -> (Float,Float)
somaDuplo (x,y) (a,b) = (x + a , y + b)

isCre :: Movimento -> Bool 
isCre (Credito _) = True
isCre (Debito _) = False 

valorMovimento :: Movimento -> Float 
valorMovimento (Credito valor) = valor 
valorMovimento (Debito valor) = valor
