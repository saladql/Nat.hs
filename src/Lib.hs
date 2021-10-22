{-# LANGUAGE GADTs,RankNTypes #-}
module Lib where
import Prelude hiding (succ)
someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Digit n = (Num n, Eq n, Show n) => n

data Nat where
  S     :: (Eq n, Eq z, Show n, Show z, Num n, Num z) => Digit z -> Digit n -> Nat
  Z     :: () -> Nat
  NZ    :: Nat -> Nat
  NS    :: (Digit n -> Digit n -> Nat) -> Nat -> Nat
  DIVzZ :: () ->  Nat

instance Num Nat where
instance Eq Nat where

-- showsPrec d x r ++ s  ==  showsPrec d x (r ++ s)
instance Show Nat where 
  showsPrec d (S z n)  = showsPrec d z . showsPrec d " + " . showsPrec (d+1) n
  showsPrec d (Z _)    = showsPrec d "0)"
  showsPrec d (NZ _)   = showsPrec d "("
  showsPrec d (NS _ m) = showsPrec d "(" . showsPrec (d `seq` d+1) m . showsPrec d ")"
  showsPrec d (DIVzZ _)= showsPrec d "<<divding by zero already, son?>>"


(>>>) :: Nat -> Nat -> Nat
(>>>) z n = S z n

succ :: Nat -> Nat
succ btm@(DIVzZ _) = btm
succ f@(NZ _)      = NS (S) f
succ (NS _ o)      = NS (>>>) (succ o)
succ n             = n `seq` (succ (shrink n))


shrink :: Nat -> Nat
shrink (Z _) = DIVzZ ()

fromIntegralHelper :: forall n. (Show n, Num n, Eq n) => Digit n -> (Nat, n)
fromIntegralHelper = \n -> foldl (\(b,a) _ -> if a == 0 then (b,0) else (succ b, a `seq` a-1)) (NZ(Z ()),n) (repeat ())
fromIntegral :: forall n. (Show n, Num n, Eq n) =>  Digit n -> Nat
fromIntegral = fst . fromIntegralHelper

data VDiv where
  VDiv :: Nat -> Nat -> Nat -> VDiv
