data DOrd = Infix | Praefix | Postfix |
            GInfix | GPraefix | GPostfix

data BTree = Nil |
             BNode Int BTree BTree deriving (Eq,Ord,Show)

t1 = Nil
t2 = BNode 2 (BNode 3 Nil Nil) (BNode 5 Nil Nil)

{- 1 -}

flatten :: BTree -> DOrd -> [Int]
flatten Nil _ = []
flatten (BNode v l r) Infix = flatten l Infix ++ [v] ++ flatten r Infix
flatten (BNode v l r) Praefix = [v] ++ flatten l Praefix ++ flatten r Praefix
flatten (BNode v l r) Postfix = flatten l Postfix ++ flatten r Postfix ++ [v]
flatten n GInfix = reverse $ flatten n Infix
flatten n GPraefix = reverse $ flatten n Praefix
flatten n GPostfix = reverse $ flatten n Postfix

{- 2 -}

isST :: BTree -> Bool
isST Nil = True
isST n = and $ zipWith (<) flattened (tail flattened)
    where
    flattened = flatten n Infix

{- 3 -}

type Control = String
type Func = Integer -> Integer
type Data = Integer

data Tree = Leaf Func |
            Node Func Tree Tree Tree

mkControl :: String -> Control
mkControl = filter (flip elem ['l', 'm', 'r'])

apply :: Control -> Data -> Tree -> Integer
apply "" d (Node f l m r) = f d -- empty control
apply _ d (Leaf f) = f d        -- reached leaf
apply s d (Node f l m r)
    | c == 'l' = apply cs (f d) l
    | c == 'm' = apply cs (f d) m
    | otherwise = apply cs (f d) r
    where
    (c:cs) = mkControl s

{- 4 -}

data LTree = LNode Integer [LTree] deriving (Show,Eq)

mapLT :: Func -> LTree -> LTree
mapLT f (LNode v ns) = LNode (f v) (map (mapLT f) ns)
