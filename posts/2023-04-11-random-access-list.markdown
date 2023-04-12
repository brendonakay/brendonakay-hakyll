---
title: Random Access Lists
author: Brendon A. Kay
tags: haskell, algorithms
---

## Linear search
Fetching the kth element of a list takes Θ(k) steps. [[1]](##1)

One method for making `fetch` more efficient is using a random-access list.

With random-access lists each of the operations cons, head, tail, and fetch takes logarithmic time in the length of the list, that is, O(log n) steps for a list of length n. [[1]](##1)

A random-access list is constructed out of two data structures.
The first is a binary tree:
```Haskell
data Tree a = Leaf a | Node Nat (Tree a) (Tree a)
```
Thee `Nat` is an integer to keep the size of the tree. This improves performance
of the algorithm since we don't need to calculate the size when we access a
node.

The second data structure is a sequence of perfect trees.
```Haskell
data Digit a = Zero | One (Tree a)
```
Then we define the random-access list type as the following...
```Haskell
type RAList a = [Digit a]
```

`fromRA` converts random-access lists into standard lists. This is mentioned in
the book as an "abstraction function". Notice also we are using partial
application to join `concatMap` and the local helper function `from`.
```Haskell
fromRA::RAList a -> [a]
fromRA = concatMap from
    where
        from Zero = []
        from (One t) = fromT t
```

Now to generate some lists to fetch elements from. This should be a good use case
for benchmarks. But first we need a way of _constructing_ lists. So, here's the
definition for `consRA`, short for "construct random-access (list)".
```Haskell
consRA:: a → RAList a → RAList a
consRA x xs = consT (Leaf x) xs
consT t1 [] = [One t1]
consT t1 (Zero: xs) = One t1: xs
consT t1 (One t2: xs) = Zero: consT (node t1 t2) xs
```

# TODO
  - [ ] benchmarks
  - [ ] Conclusion
  - [ ] Revision

# References
## 1
Bird, Richard; Gibbons, Jeremy. Algorithm Design with Haskell (p. 47). Cambridge University Press. Kindle Edition.
