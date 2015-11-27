{-# LANGUAGE TypeOperators, TypeFamilies, UndecidableInstances #-}

module StaticPositional where

import Prelude hiding ((+), (*), (:))

data Zero
data One
data ShiftZero n
data ShiftOne n

type family Succ a :: * where
    Succ Zero = One
    Succ One = ShiftZero One
    Succ (ShiftZero n) = ShiftOne n
    Succ (ShiftOne n) = ShiftZero (Succ n)

type family Last a :: * where
    Last Zero = Zero
    Last One = One
    Last (ShiftZero n) = Zero
    Last (ShiftOne n) = One

type family Pred a :: * where
    Pred One = Zero
    Pred (ShiftZero One) = One
    Pred (ShiftZero n) = ShiftOne (Pred n)
    Pred (ShiftOne n) = ShiftZero n

type family Unshift a :: * where
    Unshift Zero = Zero
    Unshift One = Zero
    Unshift (ShiftZero n) = n
    Unshift (ShiftOne n) = n

infixl 6 +
type family a + b :: * where
    Zero        + n = n
    One         + n = Succ n
    ShiftZero m + n = Last n + ShiftZero (m + Unshift n)
    ShiftOne m  + n = Last n + ShiftOne (m + Unshift n)

infixl 7 *
type family a * b :: * where
    Zero        * n = Zero
    One         * n = n
    ShiftZero m * n = ShiftZero (m * n)
    ShiftOne m  * n = n + ShiftZero (m * n)

type family Fac a :: * where
    Fac Zero = One
    Fac n = n * (Fac (Pred n))

infixl 1 !!
type family a !! b :: * where
    m !! n = ShiftZero m + n

type O = Zero
type I = One
