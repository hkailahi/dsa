module Ch1.Naturals where

import Prelude (Eq, Show, Ord, ($), Int, (+), (-), (>=), (==), (*), error, take, repeat, (.), foldr)

data Nat =
    Zero
  | Succ Nat
  deriving (Show, Eq, Ord)
zero = Zero
succ = Succ
pattern One :: Nat
pattern One = Succ Zero

toNat :: Int -> Nat
toNat n =
  foldr (\f b -> f b) Zero
    . take n
    $ repeat Succ

fromNat :: Nat -> Int
fromNat = helper 0
  where
    helper :: Int -> Nat -> Int
    helper n Zero       = n
    helper n (Succ nat) = helper (n + 1) nat

-- plus : Nat <- (Nat x Nat)
plus :: (Nat, Nat) -> Nat
plus (m, Zero)   = m
plus (m, Succ n) = succ $ plus (m, n)

-- mult : Nat <- (Nat x Nat)
mult :: (Nat, Nat) -> Nat
mult (m, Zero)   = Zero
mult (m, Succ n) = plus (m, mult (m, n))

shittyFact :: Int -> Int
shittyFact = \case
  0 -> 1
  -- behold shitty n+k patterns
  n | n >= 1 -> let k = n - 1
            in  (k + 1) * shittyFact k
  otherwise -> error "lol"

shittyFib :: Int -> Int
shittyFib = \case
  0 ->  0
  1 -> 1
  -- behold shitty n+k patterns
  n | n >= 2 -> let k = n - 2
                 in shittyFib k + shittyFib (k + 1)
  otherwise  -> error "lol"

-- fact : Nat <- Nat
natFact :: Nat -> Nat
natFact = \case
  Zero   -> One
  Succ n -> mult (plus (n, One), natFact n)

-- fib : Nat <- Nat
natFib :: Nat -> Nat
natFib = \case
  Zero          -> Zero
  One           -> One
  Succ (Succ n) -> plus (natFib n, natFib $ plus (n, One))