{-# LANGUAGE TypeOperators, TypeFamilies, UndecidableInstances #-}

module StaticTypeFamilies where

import Prelude hiding ((+), (*))

data Zero
data Succ n

type One   = Succ Zero
type Two   = Succ One
type Three = Succ Two
type Four  = Succ Three

zero  = undefined :: Zero
one   = undefined :: One
two   = undefined :: Two
three = undefined :: Three
four  = undefined :: Four

type family a + b :: * where
    Zero   + n = n
    Succ m + n = Succ (m + n)

type family a * b :: * where
    Zero   * n = Zero
    Succ m * n = n + (m * n)

type family Fac a :: * where
    Fac Zero = One
    Fac (Succ m) = (Succ m) * (Fac m)