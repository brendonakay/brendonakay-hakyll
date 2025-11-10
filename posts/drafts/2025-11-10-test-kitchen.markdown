---
title: Test Kitchen - Content and Styling Tests
author: Test Author
tags: test, markdown, mathematics, code, formatting
---

# Test Kitchen

This page contains various content types to test rendering and styling.

## Code Highlighting Test

### Basic Haskell
```haskell
-- Simple function definitions
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- List operations
fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

-- Higher-order functions
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs
```

### Type Classes
```haskell
-- Functor instance for Maybe
instance Functor Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just (f x)

-- Custom type class
class Describable a where
  describe :: a -> String

instance Describable Bool where
  describe True  = "This is true"
  describe False = "This is false"
```

## Mathematics Test

### Inline Math
Here's some inline math: \(E = mc^2\) and \(\sum_{i=1}^{n} x_i\).

### Display Math  
\[\int_{-\infty}^{\infty} e^{-x^2} dx = \sqrt{\pi}\]

\[f(x) = \begin{cases}
x^2 & \text{if } x \geq 0 \\
-x^2 & \text{if } x < 0
\end{cases}\]

## Text Formatting

This is **bold text** and this is *italic text*. Here's some `inline code`.

### Lists

- First item
- Second item with `inline code`
- Third item

### Tables

| Data Structure | Time Complexity |
|---------------|----------------|
| List | O(n) access |
| Map | O(log n) |

## End Test

This concludes the basic test content.