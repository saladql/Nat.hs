{-# LANGUAGE GADTs,RankNTypes #-}
module Lib
    ( someFunc
    ) where
import Prelude hiding (succ)
someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Digit n = (Num n, Eq n) => n

data Nat where
  S     :: Digit z -> Digit n -> Nat
  Z     :: () -> Nat
  NZ    :: Nat -> Nat
  NS    :: (Digit n -> Digit n -> Nat) -> Digit n -> Nat
  DIVzZ :: () ->  Nat


succ :: Nat -> Nat
succ btm@(DIVzZ _) = btm
succ f@(NZ _)      = NS (S) f
succ n = n `seq` (succ (shrink n))


shrink :: Nat -> Nat
shrink (Z _) = DIVzZ ()

fromIntegralHelper :: forall n. (Num n, Eq n) => Digit n -> (Nat, n)
fromIntegralHelper = \n -> foldl (\(b,a) _ -> if a == 0 then (b,0) else (succ b, n `seq` n -1)) (Z (),n) (repeat ())
fromIntegral :: forall n. (Num n, Eq n) =>  Digit n -> Nat
fromIntegral = fst . fromIntegralHelper

data VDiv where
  VDiv :: Nat -> Nat -> Nat -> VDiv
