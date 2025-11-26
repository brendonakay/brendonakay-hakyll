---
title: Using Haskell to Compute Dijkstra's Shortest Path Algorithm
author: Brendon
tags: mathematics, haskell, algorithms
---

## Introduction

For this series of posts I will cover the mathematics as well as implementing,
in Haskell, shortest path algorithms. In the final post I will cover a
topic that has piqued my interest lately, Algebraic Path Problems. First, we
will begin with Dijkstra's.

## Mathematics of the Classical Shortest Path

The following is directly from _Path Problems in Networks[1]_:

> We are given a directed graph \(G = (V, E)\) with a real-valued edge weight
> function \(w : E \rightarrow \mathbb{R} \cup \{\infty\}\). We will assume that
> all possible edges in \(V \times V\) exist, but some may have a weight of
> \(\infty\).
> 
> We define the weight of a path \(p = (v_0, v_1, \ldots, v_k)\) to be the sum of
> the weights of its edges:
> 
> \[\text{weight}(p) = \sum_{i=0}^{k-1} w(v_i, v_{i+1})\]
> 
> The shortest path weight from vertex \(u\) to vertex \(v\) is the smallest path
> weight among all paths that start at \(u\) and end at \(v\):
> 
> \[\delta(u, v) = \min_{p \in P_{uv}} w(p)\]

These two equations are important and will show up later in the Algebraic Path
Problem.

TODO: 
 - Uniqueness and Existence
 - SSSP / APSP

## Dijkstra's Algorithm Pseudocode

We should note that Dijkstra's requires non-negative weights on edges.

\(\text{Dijkstra}(G, w, s)\)

```
1  d[s] ← 0
2  for v ∈ V \ {s}
3    do d[v] ← ∞
4  Q ← V
5  while Q ≠ ∅
6    do u ← v s.t. d[v] = min{d[x] | x ∈ Q}
7       Q ← Q \ {u}
8       for each vertex v ∈ Q
9         do d[v] ← min{d[v], d[u] + w(u, v)}
```

## Haskell Implementation

First, some imports and type definitions.

```haskell
module Dijkstra (dijkstra, Graph, Weight, Vertex) where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

type Vertex = Int

type Weight = Int

type Graph = Map.Map Vertex [(Vertex, Weight)]
```

The type signature for our function computing dijkstra's shortest path algorithm

```haskell
-- Given a graph and source vertex, returns shortest distances to all vertices
dijkstra :: Graph -> Vertex -> Map.Map Vertex Weight
dijkstra graph source = dijkstraHelper initialDistances unvisited
```

Function implementation, steps 1 through 4 in the algorithm.
```haskell
  where
    -- Step 1-4: Initialize distances and queue
    vertices =
      Set.fromList $
        source : concatMap (\(v, edges) -> v : map fst edges) (Map.toList graph)
    initialDistances =
      Map.insert source 0 $
        Map.fromSet (const maxBound) vertices -- d[s] ← 0, d[v] ← ∞
    unvisited = vertices -- Q ← V
```

Now we implement a helper function that we will use recursively to drain the queue.

```haskell
    dijkstraHelper :: Map.Map Vertex Weight -> Set.Set Vertex -> Map.Map Vertex Weight
    dijkstraHelper distances queue
      | Set.null queue = distances -- while Q ≠ ∅
      | otherwise =
          let -- Step 6: u ← v s.t. d[v] = min{d[x]|x ∈ Q}
              currentVertex =
                minimumBy
                  ( \v1 v2 ->
                      compare
                        (Map.findWithDefault maxBound v1 distances)
                        (Map.findWithDefault maxBound v2 distances)
                  )
                  queue
              currentDistance =
                Map.findWithDefault maxBound currentVertex distances
              neighbors = fromMaybe [] (Map.lookup currentVertex graph)
              -- Step 9: d[v] ← min{d[v], d[u] + w(u, v)}
              newDistances =
                foldl (relaxEdge currentDistance) distances neighbors
              -- Step 7: Q ← Q \ {u}
              newQueue = Set.delete currentVertex queue
           in dijkstraHelper newDistances newQueue
```

## Resources

1. https://www.vitalsource.com/products/path-problems-in-networks-john-baras-george-v9783031799839
