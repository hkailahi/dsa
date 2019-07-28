module Ch1.Naturals where

import qualified Prelude as P
import Prelude (Eq, Show, Ord, ($), Int, (-), (>=), (==), error, take, repeat, (.), foldr, Monoid(mappend, mempty), Semigroup((<>)), Foldable)

data Nat =
    Zero
  | Succ Nat
  deriving (Show, Eq, Ord)
zero = Zero
succ = Succ
pattern One :: Nat
pattern One = Succ Zero

instance Semigroup Nat where
  (<>) :: Nat -> Nat -> Nat
  (<>) n1 Zero     = n1
  (<>) Zero n2     = n2
  (<>) n1 One      = Succ n1
  (<>) One n2      = Succ n2
  (<>) (Succ n) n2 = n `mappend` Succ n2


instance Monoid Nat where
  mempty = Zero

  mappend :: Nat -> Nat -> Nat
  mappend n1 Zero     = n1
  mappend Zero n2     = n2
  mappend n1 One      = Succ n1
  mappend One n2      = Succ n2
  mappend (Succ n) n2 = n `mappend` Succ n2
  

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
    helper n (Succ nat) = helper (n P.+ 1) nat

-- plus : Nat <- (Nat x Nat)
plus :: (Nat, Nat) -> Nat
plus (m, Zero)   = m
plus (m, Succ n) = succ $ plus (m, n)

-- mult : Nat <- (Nat x Nat)
mult :: (Nat, Nat) -> Nat
mult (m, Zero)   = Zero
mult (m, Succ n) = plus (m, mult (m, n))

(+) = P.curry plus
infixl 6 +

(*) = P.curry mult
infixl 7 *

shittyFact :: Int -> Int
shittyFact = \case
  0 -> 1
  -- behold shitty n+k patterns
  n | n >= 1 -> let k = n - 1
            in  (k P.+ 1) P.* shittyFact k
  otherwise -> error "lol"

shittyFib :: Int -> Int
shittyFib = \case
  0 ->  0
  1 -> 1
  -- behold shitty n+k patterns
  n | n >= 2 -> let k = n - 2
                 in shittyFib k P.+ shittyFib (k P.+ 1)
  otherwise  -> error "lol"

-- fact : Nat <- Nat
natFact :: Nat -> Nat
natFact = \case
  Zero   -> One
  Succ n -> (n + One) * natFact n

-- fib : Nat <- Nat
natFib :: Nat -> Nat
natFib = \case
  Zero          -> Zero
  One           -> One
  Succ (Succ n) -> natFib n + natFib (n + One)

nat :: r -> (Nat -> r) -> Nat -> r
nat z s Zero     = z
nat z s (Succ n) = s n

-------------------------------------------------------------------------------
-- Currying + structural recursion

-- Homomorphism of `Nat`
foldNat :: (r, (r -> r)) -> Nat -> r
foldNat (z, s) = nat z (s . foldNat (z,s))

-- We can define curried versions of addition, multiplication and exponentiation. 
-- Currying is essential since `foldNat` gives us no way of defining recursive
-- functions on pairs of numbers

-- >>> fromNat $ plus' (toNat 5) (toNat 5)
-- 10
plus' :: Nat -> Nat -> Nat
plus' m = foldNat (m, Succ)

-- >>> fromNat $ mult' (toNat 5) (toNat 5)
-- 25
mult' :: Nat -> Nat -> Nat
mult' m = foldNat (Zero, plus' m)

-- >>> fromNat $ expn' (toNat 5) (toNat 5)
-- 3125
expn' :: Nat -> Nat -> Nat
expn' m = foldNat (One, mult' m)
