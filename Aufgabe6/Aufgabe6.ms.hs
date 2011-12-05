-- 1

data DOrd = Infix | Praefix | Postfix |
            GInfix | GPraefix | GPostfix
data BTree = Nil |
             BNode Int BTree BTree deriving (Eq,Ord,Show)

{-|flatten:
 - Traverse the BTree in the specified order.
 -}
flatten :: BTree -> DOrd -> [Int]
flatten Nil _ = []
flatten (BNode n l r) Infix    = (flatten l Infix) ++ [n] ++ (flatten r Infix)
flatten (BNode n l r) Praefix  = [n] ++ (flatten l Praefix) ++ (flatten r Praefix)
flatten (BNode n l r) Postfix  = (flatten l Postfix) ++ (flatten r Postfix) ++ [n]
flatten (BNode n l r) GInfix   = (flatten r GInfix) ++ [n] ++ (flatten l GInfix)
flatten (BNode n l r) GPraefix = (flatten r GPraefix) ++ (flatten l GPraefix) ++ [n]
flatten (BNode n l r) GPostfix = [n] ++ (flatten r GPostfix) ++ (flatten l GPostfix)

-- 2

{-|isST:
 - Returns True if the BTree is a search tree, False otherwise.
 -}
isST :: BTree -> Bool
isST Nil = True
isST n = sorted $ flatten n Infix
  where sorted (x:y:ys) = x < y && sorted (y:ys)
        sorted _ = True

-- 3

type Control = String
type Func = Integer -> Integer
type Data = Integer
data Tree = Leaf Func
          | Node Func Tree Tree Tree

{-|mkControl:
 - Removes any characters which are not 'l', 'm' or 'r'.
 -}
mkControl :: String -> Control
mkControl s = [ x | x<-s, x `elem` ['l','m','r']] 

{-|apply:
 - Applies every f on the Path specified by s to d, starting with the one in the
 - root. If not all steps of the path exist in the tree those that do are applied.
 - If the path is empty the function in the root is applied.
 -}
apply :: Control -> Data -> Tree -> Integer
apply s d (Leaf f) = f d
apply s d (Node f l m r)
  | s == []   = f d
  | c == 'l'  = apply cs (f d) l
  | c == 'm'  = apply cs (f d) m
  | c == 'r'  = apply cs (f d) r
  where (c:cs) = mkControl s

-- 4

data LTree = LNode Integer [LTree] deriving Show

{-|mapLT:
 - Applies f to the number in every node of the tree.
 -}
mapLT :: Func -> LTree -> LTree
mapLT f (LNode n c) = LNode (f n) (map (mapLT f) c)
