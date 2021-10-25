{-# LANGUAGE GADTs,LambdaCase,RankNTypes,TypeOperators#-}
module Lib where
import Prelude (Num(..)) 
import Prelude hiding (succ, fromIntegral)
import qualified Prelude
import Data.Function ((&))
import Debug.Trace
type Digit n = (Eq n, Show n) => n 
type VApp vapp = forall vapp. (vapp -> vapp) -> vapp
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
  abs         = undefined
  signum      = undefined
  

instance Enum Nat where
  toEnum 0            = NZ   zero
  toEnum 1            = NS S (zero >>> one)
  toEnum 2            = NS S (one  >>> one)
  toEnum 3            = toEnum 2        >>> toEnum 1
  toEnum n            = toEnum (pred n) >>> toEnum 1
    
    
  fromEnum (NZ _)          = 0
  fromEnum (NS _ (n `S` z))= p +  q
    where
      p = undefined
      q = undefined


instance Eq Nat where
  (Z _   ) == (Z _)    = True
  (NZ z  ) == (NZ n)   = z == n 
  (NS _ z) == (NS _ n) = z == n
  n == z               = False

(>>>) :: Nat -> Nat -> Nat
(>>>) z n = S z n

instance Semigroup Nat where   (<>) = (>>>)
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

growL ,growR ,succ ,shrinkL ,shrinkR :: Nat -> Nat

succ btm@(DIVzZ _)   = btm
succ z@(NZ _)        = NS (S) z
succ i@(NS _ o)      = NS (S) (one `S` o)

shrinkL (Z ()  )         = Z ()
shrinkL z@(NZ _)         = z
shrinkL (NS _ car)       = NS (S) (negate_one `S` car)

shrinkR (Z ()  )         = Z ()
shrinkR z@(NZ _)         = z
shrinkR (NS _ car)       = NS (S) (car `S` negate_one )


isExhuasted = \case
	   (NZ _) -> True
	   _     -> False

growL (NZ _                ) = (NZ (Z()))
growL (NS _ (p `S` q))       = NS (S) (two >>> two)
  where two = (one >>> one)
        one = p `S` p

growR (NZ _                ) = (NZ (Z()))
growR (NS _ (p `S` q))       = NS (S) (two >>> two)
  where two = (one >>> one)
        one = q `S` q

data VPow where
  VPowRx :: Nat  -> VPow
  VPowb  :: Nat  -> VPow
  VPowa  :: Nat  -> VPow
  VPApp  :: VPow -> VPow -> VPow
(VPowb b) `VPApp` (VPowRx rx) `VPApp` (VPowa a) 
  | isExhuasted rx= (VPowa a)
  | otherwise     = (VPowb (growR b)) `VPApp` (VPowRx (shrinkL rx)) `VPApp` (VPowa a) 
            

data VDiv where 
  VDivp :: Nat -> VDiv
  VDivq :: Nat -> VDiv
  VDivm :: Nat -> VDiv
  VDApp :: VDiv -> VDiv -> VDiv
  VDiv  :: Nat -> VDiv
  VDivF :: (VDiv -> VDiv) -> VDiv

(VDivp p) `VDApp` (VDivq q) `VDApp` (VDivm m) `VDApp` (VDiv d)
  | pExhausted p && qExhausted q = VDivm zero
  | pExhausted p = (VDivm m)
  | qExhausted q = (VDivp p)          `VDApp` (VDivq d)          `VDApp` (VDivm (succ m)) `VDApp` (VDiv d)
  | otherwise    = (VDivp (shrinkL p)) `VDApp` (VDivq (shrinkL q)) `VDApp` (VDivm m)        `VDApp` (VDiv d)
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
   (VDivm modulo) = x `seq` p `VDApp` q `VDApp` (VDivm mempty) `VDApp` q
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
