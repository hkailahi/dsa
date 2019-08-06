module Ch1.S3.Lists where

import qualified Prelude as P
import Prelude hiding ()
import Data.Bool (bool)

import Ch1.S2.Naturals (plus', mult')


data ListR a = NilR | Cons a (ListR a) deriving (Show, Eq)
data ListL a = NilL | Snoc (ListL a) a deriving (Show, Eq)

convertR NilL = NilR
convertL NilR = NilL

mapList :: (a -> b) -> ListR a -> ListR b
mapList _ NilR = NilR
mapList f (Cons x xs) = Cons (f x) (mapList f xs)

-- Curried foldr (Identity of `a`, Function)
foldrList :: (b, (a, b) -> b) -> ListR a -> b
foldrList (c, h) NilR        = c              
foldrList (c, h) (Cons x xs) = h (x, (foldrList (c, h) xs))

-- Curried foldl (Identity of `b`, Function)
foldlList :: (b, (b, a) -> b) -> ListL a -> b
foldlList (c, h) NilL       = c                         
foldlList (c, h) (Snoc xs x) = h ((foldlList (c, h) xs), x)

-- Catenate
cat :: ListL a -> ListL a -> ListL a
cat x = foldlList (x, P.uncurry Snoc)

-- >>> sum' (Cons 10 (Cons 20 NilR))
-- 30
sum' :: ListR Int -> Int
sum' = foldrList (0, P.uncurry (+))

-- >>> product' (Cons 10 (Cons 20 NilR))
-- 200
product' :: ListR (Int) -> Int
product' = foldrList (1, P.uncurry (*))

-- NOTE - CONS VS SNOC CONVERSION NEEDED (NilR, NilL, blah)
-- concat' :: ListR (ListR a) -> ListR a
-- concat' = foldr (NilR, P.uncurry cat)

-- >>> length' (Cons 10 (Cons 20 NilR))
-- 2
-- >>> length' NilR
-- 0
length' :: ListR a -> Int
length' = sum' . mapList (P.const 1)

-- Any function which can be expressed as a fold after a mapping operation can also
-- be expressed as a single fold
-- >>> length'' (Cons 10 (Cons 20 NilR))
-- 2
-- >>> length'' NilR
-- 0
length'' :: ListR a -> Int
length'' = foldrList (0, (+) 1 . snd)

wrap :: a -> ListR a 
wrap a = Cons a NilR

nilp :: a -> ListR a
nilp _ = NilR

-- Inefficient
-- filter :: (a -> Bool) -> ListR a -> ListR a
-- filter p =
--   concat . mapList (\a -> if p a then (wrap a) else (nilp a))

-- Single fold
-- >>> filter' (> 10) (Cons 11 (Cons 2 (Cons 3 (Cons 99 NilR))))
-- Cons 11 (Cons 99 NilR)
filter' :: (a -> Bool) -> ListR a -> ListR a
filter' p = foldrList (NilR, keepOrTrash)
  where
    -- shitty if else because ¿¿y naut??
    keepOrTrash (x, xs) | p x         = Cons x xs
    keepOrTrash (x, xs) | P.not $ p x = xs
