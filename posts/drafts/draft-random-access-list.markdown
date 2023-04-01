---
title: MLB Stats API with Haskell
author: Brendon A. Kay
tags: haskell, algorithms
---

## Linear search
Fetching the kth element of a list takes Θ(k) steps. [[1]](##1)

One method for making `fetch` more efficient is using a random-access list.

With random-access lists each of the operations cons, head, tail, and fetch takes  logarithmic time in the length of the list, that is, O(log n) steps for a list of length n. [[1]](##1)

A random-access list is constructed out of two other data structures, the first of  which is a binary tree:
```
data Tree a = Leaf a | Node (Tree a) (Tree a)
```

# References
## 1
Bird, Richard; Gibbons, Jeremy. Algorithm Design with Haskell (p. 47). Cambridge University Press. Kindle Edition.
