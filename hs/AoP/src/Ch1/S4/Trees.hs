module Ch1.S4.Trees where

import Prelude hiding ()

data Tree' a = 
    Tip a
  | Bin (Tree' a, Tree' a)
  deriving (Show, Eq)
-- >>> bin (tip 0, bin (tip 1, tip 2))
-- Bin (Tip 0,Bin (Tip 1,Tip 2))
tip = Tip
bin = Bin

-- foldt' (f,g) : B <- Tree' A iff f : B <- A and g : B <- B x B
foldt' :: (a -> b, (b, b) -> b) -> Tree' a -> b
foldt' (f, g) = \case
  Tip a      -> f a
  Bin (x, y) -> g (foldt' (f, g) x, foldt' (f, g) y)

-- >>> size (bin (tip 0, bin (tip 1, tip 2)))
-- 3
size :: Tree' a -> Int
size = foldt' (one, uncurry (+))
  where
    one = const 1

-- >>> depth (bin (tip 0, bin (tip 1, tip 2)))
-- 2
depth :: Tree' b -> Int
depth = foldt' (zero, succ . bmax)
  where
    zero = const 0
    bmax (x, y) -- binary maximum
      | x > y     = x
      | otherwise = y

-----------------------------------------------------------------------------

data Tree a = Fork (a, Forest a) deriving (Show, Eq)
data Forest a = Null | Grow (Tree a, Forest a) deriving (Show, Eq)

foldt :: ( (a, b) -> c
         , b
         , (c, b) -> b
         )
      -> Tree a
      -> c
foldt (g, c , h) (Fork (a, xs)) = g (a, foldf (g, c, h) xs)
 
foldf :: ( (a, b) -> c
         , b
         , (c, b) -> b
         )
      -> Forest a
      -> b
foldf (g, c, h)  Null           = c
foldf (g, c, h)  (Grow (x, xs)) = h (foldt (g, c, h) x, foldf (g, c, h) xs)

-- >>> sizet (Fork (23, Grow (Fork (32, Null), Null)))
-- 2
sizet :: Tree a -> Int
sizet = foldt (succ . snd, 0, uncurry (+))