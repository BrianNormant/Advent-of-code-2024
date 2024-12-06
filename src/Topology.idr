module Topology

import Lib

import Data.List
import Data.Maybe
import Data.Either
import Data.SortedMap

import Debug.Trace
import System

export
Graph : Type -> Type
Graph a = List (List a)

-- en.wikipedia.org/wiki/Graph_theory#/media/File:Example_of_simple_undirected_graph_3.svg
ex1 : Graph Nat
ex1 = [
  [0, 1, 0, 1, 0, 0],
  [1, 0, 1, 1, 0, 0],
  [0, 1, 0, 0, 1, 0],
  [1, 1, 0, 0, 1, 0],
  [0, 0, 1, 1, 0, 1],
  [0, 0, 0, 0, 1, 0]
  ]

ex2 : Graph Nat
ex2 = [
  [0, 1, 0, 0],
  [0, 0, 0, 0],
  [0, 0, 0, 1],
  [1, 0, 0, 0]
  ]

export
||| Compute the adjacency list of a graph
||| from a list of vertices
adjList : Eq a => List (a, a) -> List (a, (List a))
adjList [] = []
adjList l = let (a, b) = splitPairs l
              in map (\x => (x, l |> filter (fst ||> (== x)) |> map snd )) ( nub (a) )

export
||| compute the in-degree of each node
||| from a list of vertices
inDegList : Eq a => List (a, a) -> List (a, Integer)
inDegList [] = []
inDegList l = let (b, a) = splitPairs l
                  al = nub (a ++ b)
               in map (\x => (x, l |> filter (snd ||> (== x)) |> length |> cast)) al

reduceA : Ord a => List (a, Integer) -> (List a, List (a, Integer))
reduceA l = let z = l |> filter (snd ||> (== 0))
                      |> sortBy (\x,y => compare (fst y) (fst x))
                      |> map Builtin.fst
                      in if (length z == 0)
                            then reduceA (map (map ((+) (-1))) (l))
                            else (z, l)


process : Ord a => List (a, List a) -> List (a, Integer) -> (List (a, List a), List (a, List a), List (a, Integer))
process a b = let (res, vert) = reduceA b
                  sel = a |> filter (\x =>
                        any (== (fst x)) res
                        )
                  --- remove processed nodes from adj list
                  nei = a |> filter (\x =>
                        any (== (fst x)) res
                        ) |> map snd
                        |> join

                  adj = foldl (\acc, el =>
                                filter (fst ||> (/= el)) acc
                        ) a res

                  --- remove processed node from indegree list
                  vert' = foldl (\acc, el =>
                          acc |> filter (fst ||> (/= el))
                          ) vert res

                  --- update indegree for neighbors
                  vert' = foldl (\acc, el =>
                          mapif (fst ||> ((==) el)) (map ((+) (-1))) acc
                          ) vert' nei
                  in (sel, adj, vert')

-- partial
topoSortInt : Ord a => List (a, List a) -> List (a, List a) -> List (a, Integer) -> List (a, List a)
topoSortInt r [] _ = r
topoSortInt r adj indeg = (process adj indeg
                        |> (\(a,b,c) => topoSortInt (r ++ a) b c))

export
topoSort : Ord a => List (a, List a) -> List (a, Integer) -> List (a, List a)
topoSort = topoSortInt []

rules : List (Integer, Integer)
rules = [
  (47, 53),
  (97, 13),
  (97, 61),
  (97, 47),
  (75, 29),
  (61, 13),
  (75, 53),
  (29, 13),
  (97, 29),
  (53, 29),
  (61, 53),
  (97, 53),
  (61, 29),
  (47, 13),
  (75, 47),
  (97, 75),
  (47, 61),
  (75, 61),
  (47, 29),
  (75, 13),
  (53, 13)
  ]

-- [(47, [53, 13, 61, 29]), (97, [13, 61, 47, 29, 53, 75]), (75, [29, 53, 47, 61, 13]), (61, [13, 53,
--  29]), (29, [13]), (53, [29, 13])]

-- [(53, 4), (13, 6), (61, 3), (47, 2), (29, 5), (75, 1), (97, 0)]

tmp : ?sdfsf
tmp = [(47, [53, 13, 61, 29]), (61, [13, 53, 29]), (29, [13]), (53, 
[29, 13])]

tmp2 : ?sdsea
tmp2 = [(53, 2), (13, 4), (61, 1), (47, 0), (29, 3)]
