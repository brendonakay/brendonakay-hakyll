---
title: Dijkstra's Shortest Path Algorithm in Haskell
author: Brendon
tags: mathematics, haskell, algorithms
---

## Introduction

For this series of posts I will cover the mathematics and computation,
using Haskell, of shortest path algorithms. In the final post I will cover a
topic that has piqued my interest lately, Algebraic Path Problems. First, we
will begin with Dijkstra's.

## Mathematics of the Single Source Shortest Path

Here I will provide some definitions and context for the shortest path problem
I will aim to calculate using Dijkstra's.

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

With Dijkstra's I will be calculating the Single Source Shortest Path Problem
(SSSP). This means I have to provide the source vertex. Computationally it is
faster than most algorithms that calculate multi-source search problems, with a
time complexity of \(O(n^2)\).

## Dijkstra's Algorithm Pseudocode

Below is the algorithmic pseudocode for Dijkstra's. I should note that
Dijkstra's requires non-negative weights on edges.

\(\text{Dijkstra}(G, w, s)\)

```
1  d[s] ← 0
2  for v ∈ V \ {s}
3    do d[v] ← ∞
4  Q ← V
5  while Q ≠ ∅
6    do u ← v s.t. d[v] = min{d[x] | x ∈ Q}
7      Q ← Q \ {u}
8      for each vertex v ∈ Q
9        do d[v] ← min{d[v], d[u] + w(u, v)}
```

## Haskell Implementation

First, some imports and type definitions. I create a union type `Distance` with
a finite or infinite weight. Then I define ordinal and numeric typeclass
instances for later calculations.

```haskell
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

type Vertex = Int

type Weight = Int

data Distance = Finite Weight | Infinity
  deriving (Eq, Show)

instance Ord Distance where
  compare Infinity Infinity = EQ
  compare Infinity _ = GT
  compare _ Infinity = LT
  compare (Finite a) (Finite b) = compare a b

instance Num Distance where
  Finite a + Finite b = Finite (a + b)
  _ + _ = Infinity

  Finite a * Finite b = Finite (a * b)
  _ * _ = Infinity

  abs (Finite a) = Finite (abs a)
  abs Infinity = Infinity

  signum (Finite a) = Finite (signum a)
  signum Infinity = Finite 1

  fromInteger n = Finite (fromInteger n)

  negate (Finite a) = Finite (negate a)
  negate Infinity = Infinity

type Graph = Map.Map Vertex [(Vertex, Weight)]
```

Next is the type signature for the function computing dijkstra's shortest path
algorithm. I take as parameters a `Graph` and source `Vertex` and compute the
distances.

```haskell
-- Given a graph and source vertex, returns shortest distances to all vertices
dijkstra :: Graph -> Vertex -> Map.Map Vertex Distance
dijkstra graph source = dijkstraHelper initialDistances unvisited
```

Now I will provide the initial body of the function implementation, steps 1
through 4 in the algorithm. The vertices are computed by converting the graph
into a list of tuples of vertices and edges, appending those to the source
vertex. I dedupe by converting to a set. Initial distances are set to infinity
to indicate they have not been visited; i.e. a weight of infinity. The source
gets 0 for no distance.

```haskell
  where
    -- Step 1-4: Initialize distances and queue
    vertices =
      Set.fromList $
        source : concatMap (\(v, edges) -> v : map fst edges) (Map.toList graph)
    initialDistances =
      Map.insert source (Finite 0) $
        Map.fromSet (const Infinity) vertices -- d[s] ← 0, d[v] ← ∞
    unvisited = vertices -- Q ← V
```

Now I implement a helper function that I will use recursively to drain the
queue, computing minimum distances along the way.

```haskell
    dijkstraHelper ::
      Map.Map Vertex Distance ->
      Set.Set Vertex ->
      Map.Map Vertex Distance
    dijkstraHelper distances queue
      | Set.null queue = distances
      -- while Q ≠ ∅
      | otherwise =
          let -- Step 6: u ← v s.t. d[v] = min{d[x]|x ∈ Q}
              currentVertex =
                -- Aggregate function to calculate delta
                minimumBy
                  ( \v1 v2 ->
                      compare
                        (Map.findWithDefault Infinity v1 distances)
                        (Map.findWithDefault Infinity v2 distances)
                  )
                  queue
              currentDistance =
                Map.findWithDefault Infinity currentVertex distances
              neighbors = fromMaybe [] (Map.lookup currentVertex graph)
              -- Step 9: d[v] ← min{d[v], d[u] + w(u, v)}
              newDistances =
                foldl (relaxEdge currentDistance) distances neighbors
              -- Step 7: Q ← Q \ {u}
              newQueue = Set.delete currentVertex queue
           in dijkstraHelper newDistances newQueue
```

These are the two functions to implement edge relaxation (i.e. updating the
shortest path) and minimum comparison.

```haskell
    relaxEdge ::
      Distance ->
      Map.Map Vertex Distance ->
      (Vertex, Weight) ->
      Map.Map Vertex Distance
    relaxEdge currentDist distances (neighbor, edgeWeight) =
      let newDistance = currentDist + Finite edgeWeight -- d[u] + w(u, v)
          oldDistance = Map.findWithDefault Infinity neighbor distances
       in if newDistance < oldDistance -- min{d[v], d[u] + w(u, v)}
            then Map.insert neighbor newDistance distances
            else distances

    minimumBy :: (a -> a -> Ordering) -> Set.Set a -> a
    minimumBy cmp set = Set.fold (\x acc -> if cmp x acc == LT then x else acc) (Set.findMin set) set
```

In the final version I implemented an enhanced algorithm that
tracks predecessors for path reconstruction. This way I can calculate both the
shortest distances from the source vertex as well as the shortest path
between two vertices. With the predecessors enhancement, I account for the
previous vertex I came from (our predecessor). If you would like to see the
final code, check out [the repo](https://github.com/brendonakay/algebraic-path-problem).

## Conclusion

It's always refreshing brushing up on classical algorithms like Dijkstra's.
It's extra enjoyable for me to implement them in Haskell. I appreciate the
language's use of pattern matching and algebraic data types to structure the
problem, and higher order functions allow for implementing a line of
algorithmic pseudocode with a one liner that looks very similar.

Thanks for reading along. Next I will implement Bellman-Ford in Haskell.
One topic I want to cover further is optimizing Dijkstra's, so hopefully I'll
make that a future post after this series. If there are any corrections or
comments please don't hesitate to reach out.

## Resources

[1] https://www.vitalsource.com/products/path-problems-in-networks-john-baras-george-v9783031799839
