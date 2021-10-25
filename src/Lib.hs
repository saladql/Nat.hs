{-# LANGUAGE GADTs,LambdaCase,RankNTypes,TypeOperators#-}
module Lib where
import Prelude (Num(..)) 
import Prelude hiding (succ, fromIntegral)
import qualified Prelude
import Data.Function ((&))
import Debug.Trace
type Digit n = (Eq n, Show n) => n

data Nat where
  S     :: (Eq n, Eq z, Show n, Show z, Num n, Num z, n ~ z) => Digit z -> Digit n -> Nat
  Z     :: () -> Nat
  NZ    :: Nat -> Nat
  NS    :: (Digit n -> Digit n -> Nat) -> Nat -> Nat
  DIVzZ :: () ->  Nat

instance Num Nat where
  fromInteger = toEnum . fromEnum . fromInteger
  (+)         = (>>>)
  (-)         = (>>>)
  (*)         = (>>>)
  

instance Enum Nat where
  toEnum 0 = NZ   zero
  toEnum 1 = NS S one
  toEnum k = NS S (succ (toEnum (k + negate 1) :: Nat))
  fromEnum (NZ zero)  = 0
  fromEnum (NS _ one) = 1
  fromEnum k          = 1 + fromEnum (shrink k) -- foldr crunch \a b b -> ta -> b (\(fromEnum (shrink k)

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

negate_one, zero, one :: Nat

-- typical integers
one         = S 0 1
zero        = Z ()
negate_one  = S 0 (negate 1)

succ :: Nat -> Nat
succ btm@(DIVzZ _)   = btm
succ z@(NZ _)        = NS (S) z
succ i@(NS _ o)      = NS (S) (one `S` o)
grow = succ


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

data Mod = RunMod Nat Nat (Nat -> Nat -> (Nat,Nat))
(//) = RunMod
infixl `RunMod` 

instance Semigroup Mod  where 
  (<>) (RunMod p' q' _) (RunMod d' b' _) = RunMod p b solver
    where
      p = (p' <> d')
      b = (q' <> b')

instance Monoid Mod where mempty = RunMod mempty mempty solver

solver :: Nat -> Nat -> (Nat, Nat)
solver p' q' =  (modulo, mempty)
  where
   p =  (VDivp p')
   q =  (VDivq q')
   (VDivm modulo) = x `seq` p `VApp` q `VApp` (VDivm mempty) `VApp` q
   x = trace "entering ..." ()

key :: Mod -> (Nat, Nat)
key (RunMod p q ouroboros) = ouroboros p q

ns = [(10 // 5), (20 // 5), (10 // 10)]

data ISO n s z where 
  Mirror :: (Eq n, Show z, Eq z, Show n) => (s ((Nat -> Nat -> (Nat, Nat)) -> s n) -> s z) -> ISO n s z


iso :: forall is s cos.
        (Functor s,
        Eq cos,
        cos ~ (s cos),
	Eq (s cos),
	Show cos,
	Show (s cos), is ~ cos)
    => ISO is s (s cos)
iso = Mirror (fmap ((&) solver))

runISO :: forall f n s z . (Applicative f, Applicative s) => Mod -> ISO n s z -> (f(s (Nat,Nat)))
runISO m (Mirror w) = fmap (fmap key) (pure (pure m))

someFunc :: IO ()
someFunc = return ()
