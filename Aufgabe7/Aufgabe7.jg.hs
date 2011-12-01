import Data.List ((\\), nub, intersect, union, sort)

{- Types for 1 and 2 -}

type Vertex = Integer
type Origin = Vertex
type Destination = Vertex
type Key = Integer
type Name = Integer

data BTree a = BLeaf Key a |
               BNode Key a (BTree a) (BTree a) deriving Show
data LTree a = LNode Key a [(LTree a)] deriving Show
data ALgraph = ALg [(Origin,[Destination])] deriving (Eq,Show)

{- 1 -}

{- Two BTrees are equal exactly when structure and values match -}

instance Eq a => Eq (BTree a) where
    (BNode _ a l r) == (BNode _ a' l' r') = a == a' && l == l' && r == r'
    (BLeaf _ a) == (BLeaf _ a') = a == a'
    _ == _ = False

{- Two LTrees are equal exactly when structure and values match -}

instance Eq a => Eq (LTree a) where
    (LNode _ a cs) == (LNode _ a' cs') = a == a' && cs == cs'

{- 2 -}

class Structure s where
    noOfSources :: s -> Integer
    noOfSinks :: s -> Integer
    notSourceConnected :: s -> [Name]
    notSinkConnected :: s -> [Name]

{- A BTree only has a single source (root) and 2^n sinks (each leaf).
 - There are no unconnected elements -}

instance Structure (BTree a) where
    noOfSources _ = 1
    noOfSinks (BLeaf _ _) = 1
    noOfSinks (BNode _ _ l r) = noOfSinks l + noOfSinks r
    notSourceConnected _ = []
    notSinkConnected _ = []

{- A LTree only has a single source (root), and all leafs are sinks.
 - There are no unconnected elements -}

instance Structure (LTree a) where
    noOfSources _ = 1
    noOfSinks (LNode _ _ []) = 1
    noOfSinks (LNode _ _ cs) = sum $ map noOfSinks cs
    notSourceConnected _ = []
    notSinkConnected _ = []

{- ALgraph:
 - Sources are all names occurring in any Origin but no Destination.
 - Sinks are all names occuring in any Destination but no Origin.
 - -}

{- All destinations in a graph. -}

dests :: ALgraph -> [Destination]
dests (ALg adj) = (nub . concat . snd . unzip) adj

{- All origins in a graph. -}

origs :: ALgraph -> [Origin]
origs (ALg adj) = (nub . fst . unzip) adj

{- All nodes without incoming edges (= origins \\ dests) -}

sources :: ALgraph -> [Origin]
sources x = (origs x) \\ (dests x)

{- All nodes without outgoing edges -}

sinks :: ALgraph -> [Destination]
sinks (ALg adj) =  origs $ ALg $ filter ((== []) . snd) adj

{- Returns all unreachable nodes from
 - ns: a node pool
 - cur: all current positions
 - step: function which returns next positions from cur -}

unreachable :: Eq a => ([a] -> [a]) -> [a] -> [a] -> [a]
unreachable _ [] _ = []
unreachable _ ns [] = ns
unreachable step ns cur = unreachable step (ns \\ cur) (step cur `intersect` (ns \\ cur))

instance Structure (ALgraph) where
    -- a \\ b is the set operation "a except b"
    noOfSources x = toInteger $ length $ sources x
    noOfSinks x = toInteger $ length $ sinks x
    -- Start with a list A of all nodes and a list S of source nodes;
    -- A = A \\ S; S = all destinations of nodes in S `intersect` A;
    -- Repeat until A is empty or S is empty.
    notSourceConnected x@(ALg adj) = sort $ unreachable allDests (union (origs x) (dests x)) (sources x)
        where allDests o = dests $ ALg (filter ((`elem` o) . fst) adj)
    -- Start with a list A of all nodes and a list S of sink nodes;
    -- A = A \\ S; S = all origin nodes leading to nodes in S `intersect` A;
    -- Repeat until A is empty or S is empty.
    notSinkConnected x@(ALg adj) = sort $ unreachable allOrigs (union (origs x) (dests x)) (sinks x)
        where allOrigs o = origs $ ALg (filter ((/= []) . (`intersect` o) . snd) adj)

{- Types for 3 -}

type State = Integer
type StartState = State
type AcceptingStates = [State]
type Word a = [a]
type Row a = [[a]]

data AMgraph a = AMg [(Row a)] deriving (Eq,Show)

type Automaton a = AMgraph a

{- 3 -}

{- Given an automaton, a current state, and a word,
 - returns all possible next states. -}

nextStates :: Eq a => (Automaton a) -> State -> (Word a) -> [State]
nextStates _ _ [] = []
nextStates (AMg t) ss w = [i | (i,j) <- indexedRow, elem (head w) j]
    where row = t !! (fromIntegral ss)
          indexedRow = zip [0 .. ] row

{- Returns whether the given word is accepted by the nondeterministic,
 - finite Automaton defined by adjacency matrix t, starting state ss
 - and accepting states as. -}

accept :: Eq a => (Automaton a) -> StartState -> AcceptingStates -> (Word a) -> Bool
accept _ ss as [] = ss `elem` as
accept t@(AMg m) ss as w
    | ss < 0 || ss >= (fromIntegral $ length m) = False
    | otherwise = or $ map accept' (nextStates t ss w)
    where accept' ss' = accept t ss' as (tail w)
