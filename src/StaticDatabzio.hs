{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, OverlappingInstances, AllowAmbiguousTypes #-}

module StaticDatabzio where

data True
data False

data A
data B
data C
data AA
data BA
data BB
data CA
data CB
data AAA
data AAB
data BBA
data CBA
data CBB

class Parent a b | a -> b where
    parent :: a -> b

instance Parent AA  A
instance Parent BA  B
instance Parent BB  B
instance Parent CA  C
instance Parent CB  C
instance Parent AAA AA
instance Parent AAB AA
instance Parent BBA BB
instance Parent CBA CB
instance Parent CBB CB

class IsParent a b c | a b -> c where
    isParent :: a -> b -> c

instance Parent a b => IsParent a b True

class IsAncestor a b c | a b -> c where
    isAncestor :: a -> b -> c

instance (IsParent a b x, Parent a c, IsParent c b y, Or x y z) => IsAncestor a b z

class Or a b c | a b -> c where
    or :: a -> b -> c

instance Or False a a
instance Or True b True

u = undefined

---- addition, a la Prolog
--
--class Add a b c | a b -> c where
--  add :: a -> b -> c
--
--instance              Add  Zero    b  b
--instance Add a b c => Add (Succ a) b (Succ c)
--
--class Fac a b | a -> b where
--  fac :: a -> b
--
--instance                                Fac  Zero    One
--instance (Fac n k, Mul (Succ n) k m) => Fac (Succ n) m
