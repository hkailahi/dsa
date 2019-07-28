module Ch1.S1.Datatypes where

import Prelude (Show, Eq) -- hiding (Bool (False, True), Char, not)

data Bool = 
    False
  | True
  deriving (Show, Eq)

data Char =
    Ascii0
  | Ascii1
  | Ascii2
  -- ...
  | Ascii128
  deriving (Show, Eq)

-- 130 inhabitants
data EitherBC =
    EBool Bool -- 2 inhabitants
  | EChar Char -- 128 inhabitants
  deriving (Show, Eq)
bool = EBool :: Bool -> EitherBC
char = EChar :: Char -> EitherBC

-- 256 inhabitants
data BothBC =
  Tuple
    Bool -- 2 inhabitants
    Char -- 128 inhabitants
  deriving (Show, Eq)
tuple = Tuple :: Bool -> Char -> BothBC

not :: Bool -> Bool
not False = True
not True = False

switch :: BothBC -> BothBC
switch (Tuple b c) = Tuple (not b) c

and :: (Bool, Bool) -> Bool
and (False, b) = False
and (True,  b) = b

curriedAnd :: Bool -> (Bool -> Bool)
curriedAnd False b = False
curriedAnd True  b = b

-- Generalize
--   and  : Bool <- (Bool x Bool)
--   cand : (Bool <- Bool) <- Bool
-- with
--   f :  A <- (B x C)
--   f : (A <- B) <- C
--   f : (A <- C) <- B

-- curry : ((A <- C) <- B) <- (A <- (B x C))
curry :: ((b,c) -> a)
      -> b
      -> (c -> a)
curry f b c = f (b, c)

