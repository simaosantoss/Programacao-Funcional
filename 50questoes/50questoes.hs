module CinquentaQuestoes where

-- Questão 1 
enumFromTo' :: Int -> Int -> [Int] 
enumFromTo' n1 n2 = [n1..n2]

enumFromTo'' :: Int -> Int -> [Int]
enumFromTo'' n1 n2
    | n1 < n2 = []
    | otherwise = n1 : enumFromTo'' (n1+1) n2

-- Questão 2
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' n1 n2 n3 
    | n1 > n3 && n2 >= n1 || n1 < n3 && n2 < n1 = []
    | otherwise = n1 : enumFromThenTo' (n1+(n2-n1)) (n2+(n2-n1)) n3      -- OU -- | otherwise = n1 : enumFromThenTo n2 (2 * n2 - n1) n3

-- Questão 3
maismais :: [a] -> [a] -> [a]
maismais [] [a] = [a]
maismais [a] [] = [a]
maismais (x:xs) l2 = x : maismais xs l2

-- Questão 4
exclamacaoExclamacao :: [a] -> Int -> Maybe a
exclamacaoExclamacao [] _ = Nothing
exclamacaoExclamacao (x:xs) indice
    | indice < 0 = Nothing 
    | indice == 0 = Just x
    | otherwise = exclamacaoExclamacao xs (indice-1) 

-- Questão 5
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- Questão 6
take' :: Int -> [a] -> [a]
take' _ [] = []
take' numero (x:xs)
    | numero <= 0 = []
    | otherwise = x : take' (numero-1) xs

-- Questão 7
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' numero (x:xs)
    | numero <= 0 = x:xs
    | otherwise = drop' (numero-1) xs

-- Questão 8
zip'  :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (h:t) (h':t') = (h,h') : zip' t t'

-- Questão 9
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' numero elemento
    | numero < 0 = []
    | otherwise = elemento : replicate' (numero - 1) elemento

-- Questão 10
intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [h] = [h]
intersperse' n (x:xs) = x : n : intersperse' n xs

-- Questão 11
group' :: Eq a => [a] -> [[a]]
group' [] = [] 
group' (h:t) = insere h (group' t)

insere :: Eq a => a -> [[a]] -> [[a]]
insere x [] = [[x]]
insere x (h:t)
    | x `elem` h = (x : h) : t
    | otherwise = [x] : h : t

    -- OU --

group'' :: Eq a => [a] -> [[a]]
group'' [] = []
group'' [x] = [[x]]
group'' (h:t)
    | h `elem` (head r) = (h : (head r)) : tail r 
    | otherwise = [h] : r
        where r = group'' t

-- Questão 12
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- Questão 13
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l]

-- Questão 14
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l = l : tails' (tail l)

-- Questão 15
heads' :: [[a]] -> [a]
heads' [] = []
heads' ([]:t) = heads' t
heads' (x:xs) = head x : heads' xs

-- Questão 16
total' :: [[a]] -> Int
total' [] = 0
total' [a] = 1
total' (x:xs) = length x + total' xs

    -- OU --

total'' :: [[a]] -> Int
total'' [] = 0
total'' (h:t) = subTotal h + total'' t
    where subTotal :: [a] -> Int
          subTotal [] = 0
          subTotal (h:t) = 1 + subTotal t

-- Questão 17
fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((n1,n2,n3):n4) = (n1,n3) : fun n4

-- Questão 18
cola :: [(String,b,c)] -> String
cola [] = ""
cola ((nome,n1,n2):ns) = nome ++ cola ns

-- Questão 19
idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ [] = []
idade anoAtual idadeDesejada ((nome,nascimento):ns)
    | anoAtual - nascimento >= idadeDesejada = nome : idade anoAtual idadeDesejada ns
    | otherwise = idade anoAtual idadeDesejada ns

-- Questão 20
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n 1 = [1]
powerEnumFrom n m
    | m > 1 = powerEnumFrom n (m - 1) ++ [n^(m-1)]
    | otherwise = []

-- Questão 21                                   
isPrime :: Int -> Bool
isPrime n
    | n >= 2 = primeCheck n 2
    | otherwise = False

primeCheck :: Int -> Int -> Bool
primeCheck m n
    | m * m > n = True 
    | mod n m == 0 = False
    | otherwise = primeCheck n (m + 1)

-- Questão 22
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True 
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- Questão 23
isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] _ = True
isSuffixOf' _ [] = False
isSuffixOf' l l'@(_:t) = l == l' || isSuffixOf' l t

-- Questão 24
isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' (h:t) (h':t') = h == h' && isSubsequenceOf' t t' || isSubsequenceOf' (h:t) t'

-- Questão 25
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' x l = elemIndicesAux' x l 0

elemIndicesAux' :: Eq a => a -> [a] -> Int -> [Int]
elemIndicesAux' _ [] _ = []
elemIndicesAux' x (h:t) i
    | x == h = i : elemIndicesAux' x t (i+1)
    | otherwise = elemIndicesAux' x t (i+1)

-- Questão 26
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:t)
    | x `elem` t = nub' t 
    | otherwise = x : nub' t 

-- Questão 27
delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' x (h:t)
    | x == h = t
    | otherwise = delete' x t 

-- Questão 28
barraBarra :: Eq a => [a] -> [a] -> [a]
barraBarra [] _ = []
barraBarra l [] = l
barraBarra l (h:t) = barraBarra (delete' h l) t


-- Questão 29
union' :: Eq a => [a] -> [a] -> [a]
union' l [] = l 
union' [] ys = ys
union' l (y:ys)
    | y `elem` l = union' l ys
    | otherwise = union' (l ++ [y]) ys 

-- Questão 30 
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] _ = []
intersect' (y:ys) l
    | y `elem` l = y : intersect' ys l
    | otherwise = intersect' ys l

-- Questão 31
insert' :: Ord a => a -> [a] -> [a]
insert' a [] = [a]
insert' numero (h:t)
    | numero > h = h : insert' numero t 
    | otherwise = numero : h : t

-- Questão 32
unwords' :: [String] -> String
unwords' [] = []
unwords' [a] = a 
unwords' (h:t) = h ++ (if null t then "" else " ") ++ unwords' t

-- Questão 33
unlines' :: [String] -> String
unlines' [] = []
unlines' (h:t) = h ++ "\n" ++ unlines' t

-- Questão 34                              
pMaior :: Ord a => [a] -> Int
pMaior [_] = 0
pMaior (h:t)
    | h > t !! pMaior t = 0
    | otherwise = pMaior t+1

-- Questão 35
lookup' :: Eq a => a -> [(a,b)] -> Maybe b 
lookup' _ [] = Nothing 
lookup' elemento ((a,b):t)
    | elemento == a = Just b
    | otherwise = lookup' elemento t

-- Questão 36
preCrescente' :: Ord a => [a] -> [a]
preCrescente' [] = []
preCrescente' [x] = [x]
preCrescente' (h1:h2:t)
    | h1 <= h2 = h1 : preCrescente' (h2:t)
    | otherwise = [h1]

-- Questão 37
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort [a] = [a]
iSort (x:xs) = insert x (iSort xs)

insert :: Ord a => a -> [a] -> [a]
insert a [] = [a]
insert elemento (x:xs)
    | elemento <= x = elemento : (x:xs)
    | otherwise = x : insert elemento xs

-- Questão 38
menor :: String -> String -> Bool
menor _ "" = False
menor "" _ = True
menor (x:xs) (y:ys)
    | x < y = True 
    | x > y = False 
    | otherwise = menor xs ys

-- Questão 39
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False 
elemMSet elemento ((e,numero):es)
    | elemento == e = True
    | otherwise = elemMSet elemento es

-- Questão 40
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((e,1):es) = e : converteMSet es
converteMSet ((e,n):es) = e : converteMSet ((e,n-1):es)

-- Questão 41
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet e [] = [(e,1)]
insereMSet e ((x,n):xs)
    | e == x = (x,n+1):xs
    | otherwise = (x,n) : insereMSet e xs

-- Questão 42
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] = []
removeMSet e ((x,n):xs)
    | e == x = if n > 1 then (x, n-1) : xs else xs
    | otherwise = (x,n) : removeMSet e xs

-- Questão 43
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (l:ls) = insereMSet l (constroiMSet ls)

-- Questão 44
partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' [] = ([], [])
partitionEithers' (x:xs) =
    let (lefts,rights) = partitionEithers' xs
    in case x of
        Left a -> (a:lefts,rights)
        Right b -> (lefts, b:rights)

-- Questão 45
catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' (Just h:t) = h : catMaybes' t
catMaybes' (Nothing:t) = catMaybes' t

-- Questão 46
data Movimento = Norte | Sul | Este | Oeste
    deriving Show

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xi,yi) (xf,yf) 
    | xi < xf = Este : caminho (xi+1,yi) (xf,yf)
    | xi > xf = Oeste : caminho (xi-1,yi) (xf,yf)
    | yi < yf = Norte : caminho (xi, yi + 1) (xf, yf)
    | yi > yf = Sul : caminho (xi, yi - 1) (xf, yf)
    | otherwise = []

-- Questão 47
hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops _ [] = False
hasLoops posi movs = posi == posicao posi movs || hasLoops posi (init movs)

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao p [] = p
posicao (x, y) (Norte:t) = posicao (x, y + 1) t
posicao (x, y) (Sul:t) = posicao (x, y - 1) t
posicao (x, y) (Este:t) = posicao (x + 1, y) t
posicao (x, y) (Oeste:t) = posicao (x - 1, y) t

-- Questão 48
type Ponto = (Float,Float)
data Retangulo = Ret Ponto Ponto

contaQuadrados :: [Retangulo] -> Int
contaQuadrados [] = 0
contaQuadrados (h:t)
    | eQuadrado h = 1 + contaQuadrados t 
    | otherwise = contaQuadrados t

eQuadrado :: Retangulo -> Bool
eQuadrado (Ret (x1,y1) (x2,y2)) = abs (y2 - y1) == abs (x2 - x1)

-- Questão 49 
areaTotal :: [Retangulo] -> Float
areaTotal [] = 0 
areaTotal ((Ret (p1,p2) (p3,p4)) :t) = abs (p3-p1) * abs (p4-p2) + areaTotal t

-- Questão 50
data Equipamento = Bom | Razoavel | Avariado
    deriving Show

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (Bom:t) = 1 + naoReparar t
naoReparar (Razoavel:t) = 1 + naoReparar t
naoReparar (Avariado:t) = naoReparar t
