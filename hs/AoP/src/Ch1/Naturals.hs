module Ch1.Naturals where

import Prelude (Eq, Show, Ord, ($), Int, (+), (-), (>=), (==), (*), error)

data Nat =
    Zero
  | Succ Nat
  deriving (Show, Eq, Ord)
zero = Zero
succ = Succ

-- plus : Nat <- (Nat x Nat)
plus :: (Nat, Nat) -> Nat
plus (m, Zero)   = m
plus (m, Succ n) = succ $ plus (m, n)

-- mult : Nat <- (Nat x Nat)
mult :: (Nat, Nat) -> Nat
mult (m, Zero)   = Zero
mult (m, Succ n) = plus (m, mult (m, n))

-- fact : Nat <- Nat
fact :: Int -> Int
fact = \case
  0 -> 1
  -- behold shitty n+k patterns
  n | n >= 1 -> let k = n - 1
            in  (k + 1) * fact k
  otherwise -> error "lol"

-- fib : Nat <- Nat
fib :: Int -> Int
fib = \case
  0 ->  0
  1 -> 1
  -- behold shitty n+k patterns
  n | n >= 2 -> let k = n - 2
                 in  fib k + fib (k + 1)
  otherwise  -> error "lol"
