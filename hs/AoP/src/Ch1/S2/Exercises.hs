module Ch1.S2.Exercises where

-- Exercises

{-
1.1 - Give an example of a recursion equation that is not satisfied by any 
function.

A: `f n = (f n) + 1` is not satisfied by any function.
-}

{-
1.2 Consider the recursive equation
```
  m (x, y) = y + 1,                     if x = y,
           = m (x, m (x - 1, y + 1))    otherwise.
```
Does this determine a unique function `m`?

A: No. The following two functions obey the recursion equation:
```
  m1 (x, y) = x + 1;
  m2 (x, y) = if (x >= y) then (x + 1) else (y - 1).
```
-}