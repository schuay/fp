-- Definitions for 1 and 2

type Key = Integer

data BTree a = BLeaf Key a |
               BNode Key a (BTree a) (BTree a) deriving Show

data LTree a = LNode Key a [(LTree a)] deriving Show

-- 1a

{-|==
 - Defines the == relation for BTrees (and implicitly the /= relation).
 -}
instance Eq a => Eq (BTree a) where
  (==) (BLeaf _ a1) (BLeaf _ a2) = a1 == a2
  (==) (BNode _ a1 l1 r1) (BNode _ a2 l2 r2) = (a1 == a2) &&
                                               (l1 == l2) &&
                                               (r1 == r2)
  (==) _ _ = False -- A BNode is never equal to a BLeaf.

-- 1b

instance Eq a => Eq (LTree a) where
  (==) (LNode _ a1 c1) (LNode _ a2 c2) = (a1 == a2) &&
                                         (c1 == c2)

-- 2

-- Definitions for 2
type Vertex = Integer
type Origin = Vertex
type Destination = Vertex
type Name = Integer

data ALgraph = ALg [(Origin,[Destination])] deriving (Eq,Show)

class Structure s where
  -- Number of nodes with no inbound edges.
  noOfSources :: s -> Integer
  -- Number of nodes with no outbound edges.
  noOfSinks :: s -> Integer
  -- Nodes which cannot be reached from a source.
  notSourceConnected :: s -> [Name]
  -- Nodes from which no sink can be reached.
  notSinkConnected :: s -> [Name]

-- 2a

instance Structure (BTree a) where
  noOfSources _ = 1 -- A Tree only has one source, the root.
  noOfSinks (BLeaf _ _) = 1
  noOfSinks (BNode _ _ l r) = (noOfSinks l) + (noOfSinks r)
  notSourceConnected _ = [] -- None since there are no circles.
  notSinkConnected _ = [] -- None since there are no circles.

-- 2b

instance Structure (LTree a) where
  noOfSources _ = 1
  noOfSinks (LNode _ _ []) = 1
  noOfSinks (LNode _ _ c) = foldl (+) 0 (map (noOfSinks) c)
  notSourceConnected _ = []
  notSinkConnected _ = []


-- 2c

{-|unify:
 - Combines all lists in the first argument, while making sure that no element
 - appears more than once in the resulting list.
 -}
unify :: Eq a => [[a]] -> [a]
unify [] = []
unify (d:ds) = d ++ [x | x<-(unify ds), not (x `elem` d)]

{-|getSources:
 - Returns a list with all sources in the graph.
 -}
getSources :: ALgraph -> [Origin]
getSources (ALg als) = [x | x<-os, source x]
  where (os,ds) = unzip als
        source :: Origin -> Bool
	source o = (not . or . (map (elem o))) ds

{-|getSinks:
 - Returns a list with all sinks in the graph.
 -}
getSinks :: ALgraph -> [Destination]
getSinks (ALg als) = unify sinks
  where sinks = map (\y->([x | x<-(snd y), sink (ALg als) x])) als
	sink :: ALgraph -> Destination -> Bool
	sink (ALg []) _ = True
	sink (ALg ((o,d):r)) x = (x/=o || d==[]) && sink (ALg r) x

{-|deleteNodes:
 - Deletes all nodes in the list from the graph.
 -}
deleteNodes :: ALgraph -> [Origin] -> ALgraph
deleteNodes (ALg x) n = (ALg x')
  where (os, ds) = unzip x
        ds' = map (\y->([z | z<-y, not (z `elem` n)])) ds
	x' = [ (o,d) | (o,d)<-(zip os ds'), not (o `elem` n)]

{-|deletePathFromNode:
 - Deletes all nodes which can be reached from the nodes in the list (including
 - those in the list).
 -}
deletePathFromNode :: ALgraph -> [Origin] -> ALgraph
deletePathFromNode x [] = x
deletePathFromNode (ALg x) n = deletePathFromNode x' n'
  where x' = deleteNodes (ALg x) n
        n' = unify [(snd y) | y<-x, (fst y) `elem` n]

{-|deletePathFromNodeRev:
 - Deletes all nodes from which any node in the list can be reached (including
 - those in the list).
 -}
deletePathFromNodeRev :: ALgraph -> [Origin] -> ALgraph
deletePathFromNodeRev x [] = x
deletePathFromNodeRev (ALg x) n = deletePathFromNodeRev x' n'
  where x' = deleteNodes (ALg x) n
        n' = [(fst y) | y<-x, (or . map (\z->(z `elem` (snd y)))) n]

{-|qsort:
 - A simple Quick Sort implementation.
 -}
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where smaller = [y | y<-xs, y<=x]
        larger = [z | z<-xs, z>x]

instance Structure ALgraph where
  noOfSources x = (fromIntegral.length.getSources) x
  noOfSinks x = (fromIntegral.length.getSinks) x
  {-|notSourceConnected:
   - Deletes all nodes which can be reached from a source and all sources.
   - Whatever remains cannot be reached from a source.
   -}
  notSourceConnected (ALg x) = qsort result
    where sources = getSources (ALg x)
          (ALg x') = deletePathFromNode (ALg x) sources
	  result = unify [(snd y) | y<-x']
  {-|notSinkConnected:
   - Deletes all nodes from which a sink can be reached and all sinks.
   - From the remaining nodes no sink can be reached.
   -}
  notSinkConnected (ALg x) = qsort [(fst y) | y<-x']
    where sinks = getSinks (ALg x)
          (ALg x') = deletePathFromNodeRev (ALg x) sinks

-- 3

type State = Integer
type StartState = State
type AcceptingStates = [State]
type Word a = [a]
type Row a = [[a]]

data AMgraph a = AMg [(Row a)] deriving (Eq,Show)

type Automaton a = AMgraph a

{-|accept:
 - Determine whether the state machine accepts the word.
 -}
accept :: Eq a => (Automaton a) -> StartState -> AcceptingStates -> (Word a) -> Bool
accept (AMg rows) s e word
  | not sInA   = False -- If the start state is invalid, the entire machine is.
  | word == [] = s `elem` e -- If the word is empty, we have to be at a valid end state.
  -- Since the machine is not necessarily deterministic there my be multiple
  -- follow-up states and we have to check them all.
  | otherwise  = or (map (\x->(accept (AMg rows) x e ws)) dests)
  where maxR = fromIntegral(length rows) - 1
        sInA = s <= maxR && s >= 0 -- Is the start state valid?
	(w:ws) = word
	dests = [x | x<-[0 .. maxR], validEdge x] -- Possible follow-up states.
	{-|validEdge:
	 - An edge is valid, if the given state can be reached via it from the start
	 - state and the edge is labeled with the first part of the word.
	 -}
	validEdge :: State -> Bool
	validEdge x = w `elem` ((rows!!(fromIntegral s))!!(fromIntegral x))
