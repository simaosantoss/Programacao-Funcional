module Fichatp7PF where

data ExpInt = Const Int
              | Simetrico ExpInt
              | Mais ExpInt ExpInt
              | Menos ExpInt ExpInt
              | Mult ExpInt ExpInt
              deriving (Show)

-- exemplo para realizar testes
e :: ExpInt 
e = Mais (Const 3)
         (Mult (Const 4) (Const 5))
-- 1
--1.a
calcula :: ExpInt -> Int
calcula (Const constante) = constante
calcula (Simetrico e) = - calcula e 
calcula (Mais e1 e2) = calcula e1 + calcula e2 
calcula (Menos e1 e2) = calcula e1 - calcula e2 
calcula (Mult e1 e2) = calcula e1 * calcula e2 

-- 1.b
infixa :: ExpInt -> String
infixa (Const c) = show c
infixa (Simetrico e) = "(" ++ "-" ++ infixa e ++ ")"
infixa (Mais e1 e2) = "(" ++ infixa e1 ++ "+" ++ infixa e2 ++ ")"
infixa (Menos e1 e2) = "(" ++ infixa e1 ++ "-" ++ infixa e2 ++ ")"
infixa (Mult e1 e2) = "(" ++ infixa e1 ++ "*" ++ infixa e2 ++ ")"

-- 1.c
posfixa :: ExpInt -> String
posfixa (Const c) = show c
posfixa (Simetrico e) = posfixa e ++ " -"
posfixa (Mais e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ " +"
posfixa (Menos e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ " -"
posfixa (Mult e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ " *"

-- 2
data RTree a = R a [RTree a]
                deriving (Show)

rt :: RTree Int 
rt = R 5
     [ R 3 [] 
     , R 7 [ R 1 []
            ,R 2 []
            ]
     , R 4 [R 10 []
           ,R 5 [R 3 []]
           ,R 1 []]
    ]

-- 2.a
soma :: Num a => RTree a -> a
soma (R x []) = x 
soma (R x l) = x + sum  (map soma l)

-- 2.b 
altura :: RTree a -> Int 
altura (R _ []) = 1 
altura (R _ l) = 1 + maximum (map altura l)

--2.c  
prune :: Int -> RTree a -> RTree a 
prune 0 (R x _) = R x []
prune val (R x l) = R x (map (prune (val-1)) l)

-- 2.d 
mirror :: RTree a -> RTree a 
mirror (R x l) = R x (map mirror (reverse l))

-- 2.e                         esquerda -> direita -> topo              
postorder :: RTree a -> [a]
postorder (R x []) = [x]
postorder (R x l) = concat (map postorder l) ++ [x]

--                              topo -> esquerda -> direita
preorder :: RTree a -> [a]
preorder (R x []) = [x]
preorder (R x l) = x : concat (map preorder l)


-- 3.
data BTree a = Empty | Node a (BTree a) (BTree a)
data LTree a = Tip a | Fork (LTree a) (LTree a)

arv :: LTree Int 
arv = Fork 
           (Fork (Tip 3) 
                 (Fork (Tip 2) (Tip 1))
           )
           (Fork (Fork 
                       (Fork (Tip 4) (Tip 2))
                       (Tip 5)
                 )
                 (Fork (Tip 1) (Tip 3))
           )

-- 3.a
ltSum :: Num a => LTree a -> a
ltSum (Tip x) = x 
ltSum (Fork left right) = ltSum left + ltSum right 

-- 3.b 
listaLT :: LTree a -> [a]
listaLT (Tip x) = [x]
listaLT (Fork left right) = listaLT left ++ listaLT right

-- 3.c 
ltHeight :: LTree a -> Int
ltHeight (Tip _) = 0 
ltHeight (Fork left right) = max (ltHeight left) (ltHeight right) + 1

-- 4. 
data FTree a b = Leaf b | No a (FTree a b) (FTree a b)

arv' :: FTree Int Char 
arv' = No 3 
          (No 4 
               (Leaf 'g') 
               (Leaf 'b')
          )
          (No 1 
               (No 5 (Leaf 'd') (Leaf 'a')) 
               (Leaf 'k')
          )

-- 4.a 
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf x) = (Empty,Tip x)
splitFTree (No x left right) = (Node x bleft bright , Fork lleft lright)
       where
              (bleft , lleft) = splitFTree left
              (bright,lright) = splitFTree right 

-- 4.b 
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees Empty (Tip x) = Just (Leaf x) 
joinTrees (Node x l r) (Fork l' r') = 
    case (joinTrees l l', joinTrees r r') of 
        (Just bt, Just lt) -> Just (No x bt lt)
        _ -> Nothing 
joinTrees _ _ = Nothing 

