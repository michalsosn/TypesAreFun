{-# LANGUAGE ExplicitForAll, RankNTypes, ImpredicativeTypes, ExistentialQuantification #-}

module Quantifiers where

import Control.Monad

-- ExplicitForAll
dalength :: forall a. [a] -> Int
dalength = length
damap :: forall a b. (a -> b) -> [a] -> [b]
damap = map

-- RankNTypes
-- With explicit forall, it now becomes possible to write functions that expect polymorphic arguments, like for instance
-- Here, f is a polymorphic function, it can be applied to anything. In particular, foo can apply it to both the character 'c' and the boolean True.
foo :: (forall a. a -> a) -> (Char,Bool)
foo f = (f 'c', f True)

bar :: forall a. ((a -> a) -> (Char, Bool))
bar = const ('d', False)
-- The forall at the outermost level means that bar promises to work with any argument f as long as f has the shape a -> a
-- for some type a unknown to bar. Contrast this with foo, where it's the argument f who promises to be of shape a -> a
-- for all types a at the same time , and it's foo who makes use of that promise by choosing both a = Char and a = Bool.

-- Simple polymorphic functions like bar are said to have a rank-1 type while the type foo is classified as rank-2 type
-- There is a good reason that Haskell98 does not support higher rank types: type inference for the full System F is undecidable

-- ImpredicativeTypes
-- predicative = type variables instantiated to monotypes. impredicative = also polytypes.
-- Example: length [id :: forall a . a -> a] or Just (id :: forall a. a -> a). Subtly different from higher-rank.
-- relation of polymorphic types by their generality, i.e. `isInstanceOf`.
aa :: forall a. [[a -> Int]] -> Int  -- muszę dla każdego a, a zatem np [[Char -> Int]] lub [[Bool -> Int]], nie wiadomo, zwrócić Int
aa fs = sum $ fmap length fs

bb :: [forall a. [a -> Int]] -> Int  -- dostaję listę, w której dla każdego a są listy funkcji a -> Int, mam zwrócić int
bb [] = 0                            -- mogę dobrać a, ale potem dostaję całą podlistę tylko dla tego a
bb (fs:fss) = aux fs 1 + aux fs 'b' + bb fss
    where
        aux :: forall a. [a -> Int] -> a -> Int
        aux fs a = sum $ fmap ($a) fs

cc :: [[forall a. a -> Int]] -> Int  -- dostaję listę list funkcji a -> Int, mam zwrócić Int, ja wybieram a
cc [] = 0
cc (fs:fss) = aux fs + cc fss
    where
        aux :: [forall a. a -> Int] -> Int
        aux [] = 0
        aux (f:fs) = f 'a' + f 3 + aux fs

-- Higher-rank polymorphism (e.g. rank-N types) allows universal quantifiers to appear inside function types as well.
-- Impredicative types take this idea to its natural conclusion: universal quantifiers are allowed anywhere in a type,
-- even inside normal datatypes like lists or Maybe.
-- In mathematics and logic, a self-referencing definition is called impredicative. More precisely, a definition is said to be
-- impredicative if it invokes (mentions or quantifies over) the set being defined, or (more commonly) another set which contains the thing being defined.

-- ExistentialQuantification
-- heterogenous lists
data ShowBox = forall a. Show a => SB a

heteroList :: [ShowBox]
heteroList = [SB (), SB True, SB 10]

instance Show ShowBox where
    show (SB s) = show s

-- Firstly, forall really does mean 'for all'. One way of thinking about types is as sets of values with that type,
-- for example, Bool is the set {True, False, ⊥} (remember that bottom, ⊥, is a member of every type!)

demo :: IO ()
demo = do
    print $ dalength [3..6]
    print $ damap (*2) [0..2]
    print $ aa [[id, (*2)], [(+4), const 5]] -- mogę podać [[Int -> Int]] lub [[Char -> Int]], ja wybieram
    print $ bb [[const 1, const 2], [const 10, const 20]] -- muszę podać [] w którym będzie dla każdego a [] funkcji działająca dla tego a
    print $ cc [[const 1, const 2], [const 10, const 20]] -- muszę podać [[]] funkcji, które zadziałają dla dowolnego a
    print $ heteroList
