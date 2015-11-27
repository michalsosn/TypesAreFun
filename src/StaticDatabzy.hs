{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module StaticDatabzy where

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

type family Equ a b :: * where
    Equ a a = True
    Equ a b = False

type family Or a b :: * where
    Or True b = True
    Or False b = b

type family And a b :: * where
    And False b = False
    And True b = b

type family HasParent a :: * where
    HasParent A = False
    HasParent B = False
    HasParent C = False
    HasParent a = True

type family Parent a :: * where
    Parent AA  = A
    Parent BA  = B
    Parent BB  = B
    Parent CA  = C
    Parent CB  = C
    Parent AAA = AA
    Parent AAB = AA
    Parent BBA = BB
    Parent CBA = CB
    Parent CBB = CB

type family IsParent a b :: * where
    IsParent a b = Equ a (Parent b)

type family IsAncestor a b :: * where
    IsAncestor a b = (And (HasParent b) (Or (IsParent a b) (IsParent a (Parent b))))
