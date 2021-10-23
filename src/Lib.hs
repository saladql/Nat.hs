{-# LANGUAGE GADTs,LambdaCase,RankNTypes,TypeOperators,PolyKinds,DataKinds #-}
module Lib where
import Prelude (Num(..)) 
import Prelude hiding (succ, fromIntegral)
import qualified Prelude
type Digit n = (Eq n, Show n) => n

data Nat where
  S     :: (Eq n, Eq z, Show n, Show z, Num n, Num z, n ~ z) => Digit z -> Digit n -> Nat
  Z     :: () -> Nat
  NZ    :: Nat -> Nat
  NS    :: (Digit n -> Digit n -> Nat) -> Nat -> Nat
  DIVzZ :: () ->  Nat

instance Num Nat where

instance Enum Nat where
  toEnum 0 = NZ   zero
  toEnum 1 = NS S one
  toEnum k = NS S (succ (toEnum (k + negate 1) :: Nat))

instance Eq Nat where
  (Z _   ) == (Z _)    = True
  (NZ z  ) == (NZ n)   = z == n 
  (NS _ z) == (NS _ n) = z == n
  n == z               = False

(>>>) :: Nat -> Nat -> Nat
(>>>) z n = S z n

instance Semigroup Nat where (<>) = (>>>)
instance Monoid Nat    where mempty = NS (>>>) (zero  >>> zero)

-- showsPrec d x r ++ s  ==  showsPrec d x (r ++ s)
instance Show Nat where 
  showsPrec d (S z n)  = showsPrec d z . showsPrec d  '+' . showsPrec d n
  showsPrec d (Z _)    = showsPrec d (mempty :: String)
  showsPrec d (NZ z)   = showsPrec d '(' . showsPrec d z . showsPrec d ')'
  showsPrec d (NS _ m) = showsPrec d '(' . showsPrec (d-1) m . showsPrec d ')'
  showsPrec d (DIVzZ _)= showsPrec d "<<divding by zero already, son?>>"

zero, one :: Nat
one  = S 0 1
zero = Z ()
negate_one  = S 0 (negate 1)

succ :: Nat -> Nat
succ btm@(DIVzZ _)   = btm
succ z@(NZ _)        = NS (S) z
succ i@(NS _ o)      = NS (S) (one `S` o)

shrink :: Nat -> Nat
shrink (Z ()  )         = Z ()
shrink z@(NZ _)         = z
shrink (NS _ car)       = NS (S) (negate_one `S` car)

data VDiv where 
  VDivp :: Nat -> VDiv
  VDivq :: Nat -> VDiv
  VDivm :: Nat -> VDiv
  VApp  :: VDiv -> VDiv -> VDiv
  VDiv  :: Nat -> VDiv
  VDivF :: (VDiv -> VDiv) -> VDiv

(VDivp p) `VApp` (VDivq q) `VApp` (VDivm m) `VApp` (VDiv d)
  | pExhausted p && qExhausted q = VDivm zero
  | pExhausted p = (VDivm m)
  | qExhausted q = (VDivp p)          `VApp` (VDivq d)          `VApp` (VDivm (succ m)) `VApp` (VDiv d)
  | otherwise    = (VDivp (shrink p)) `VApp` (VDivq (shrink q)) `VApp` (VDivm m)        `VApp` (VDiv d)
  | balanced     = undefined
  where
    pExhausted = \case
                   (NZ _) -> True
                   _      -> False

    qExhausted = \case
                   (NZ _) -> True
                   _     -> False
    balanced = False

newtype Mod       = Mod { runMod :: Nat -> Nat -> Equation Nat Nat Nat Nat  }
newtype Equals    = Equals { runEquals :: Nat -> Nat -> Mod ->  (Nat `Mod` Nat) `Equals` (Nat,Nat) }
data Equation p q where
  Equation :: forall p q m d. (Digit p, Digit q) => () -> (VDivp p) `VApp` (VDivq q) `VApp` (VDivm p) `VApp` (VDiv q)

(r `Mod` q) `Equals` (q,r) :: (VDivp r) `VApp` (VDivq q) `VApp` (VDivm Nat) `VApp` (VDiv q)

someFunc = return ()
