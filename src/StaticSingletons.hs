{-# LANGUAGE GADTs, TypeOperators, DataKinds, KindSignatures, TypeFamilies #-}

module StaticSingletons where

import Prelude hiding ((+))

data Nat where
    Zero :: Nat
    Succ :: Nat -> Nat

(+) :: Nat -> Nat -> Nat
Zero   + n = n
Succ m + n = Succ (m + n)

type family (a :: Nat) :+ (b :: Nat) :: Nat where
    Zero   :+ n = n
    Succ m :+ n = Succ (m :+ n)

data SNat (a :: Nat) where
    SZero :: SNat Zero
    SSucc :: SNat a -> SNat (Succ a)

(%:+) :: SNat a -> SNat b -> SNat (a :+ b)
SZero     %:+ m = m
(SSucc n) %:+ m = SSucc (n %:+ m)


a = SSucc (SSucc SZero)
b = SSucc (SSucc (SSucc SZero))
c = a %:+ b

demo :: IO ()
demo = do
    putStrLn "hello"