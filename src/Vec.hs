{-#LANGUAGE DataKinds, KindSignatures, GADTs, TypeFamilies, ScopedTypeVariables #-}

module Vec where

import Prelude hiding (Bounded)

data Nat = Zero | Succ Nat

type N0 = Zero
type N1 = Succ N0
type N2 = Succ N1
type N3 = Succ N2
type N4 = Succ N3
type N5 = Succ N4
type N6 = Succ N5
type N7 = Succ N6
type N8 = Succ N7
type N9 = Succ N8

data NatProxy (n :: Nat) where
    NatProxy :: NatProxy n

class NatToInt (n :: Nat) where
    toInt :: NatProxy n -> Int

instance NatToInt Zero where
    toInt _ = 0

instance NatToInt n => NatToInt (Succ n) where
    toInt _ = 1 + toInt (NatProxy :: NatProxy n)

data Bounded a (n :: Nat) (m :: Nat) where
    Bounded :: [a] -> Bounded a n m

instance (Show a, NatToInt n, NatToInt m) => Show (Bounded a n m) where
    show (Bounded xs) =
        let min = toInt (NatProxy :: NatProxy n)
            max = toInt (NatProxy :: NatProxy m)
        in
            show xs ++ " in [" ++ show min ++ "," ++ show max ++ "]"

proveLength :: forall n m a . (NatToInt n, NatToInt m) =>
               [a] -> Maybe (Bounded a n m)
proveLength xs = if min <= len && len <= max
                 then Just (Bounded xs)
                 else Nothing
    where
        len = length xs
        min = toInt (NatProxy :: NatProxy n)
        max = toInt (NatProxy :: NatProxy m)

head' :: Bounded a (Succ n) m -> a
head' (Bounded (x:xs)) = x

tail' :: forall a n m . Bounded a (Succ n) (Succ m) -> Bounded a n m
tail' (Bounded (x:xs)) = Bounded xs

length' :: Bounded a (Succ n) m -> Int
length' (Bounded xs) = length xs



--shorten :: Vec a n -> Vec a (Pred n)
--shorten xs = case null' xs of
--               Null    -> xs
--               NotNull -> tail' xs


--data Vec a (n :: Nat) where
--    Nil  :: Vec a Zero
--    Cons :: a -> Vec a n -> Vec a (Succ n)
--
--data IsNull (n :: Nat) where
--    Null    :: IsNull Zero
--    NotNull :: IsNull (Succ n)
--
--type family Pred (n :: Nat) :: Nat where
--    Pred Zero     = Zero
--    Pred (Succ n) = n
--
--null' :: Vec a n -> IsNull n
--null' Nil        = Null
--null' (Cons _ _) = NotNull
--
--head' :: Vec a (Succ n) -> a
--head' (Cons hd _) = hd
--
--tail' :: Vec a (Succ n) -> Vec a n
--tail' (Cons _ tl) = tl
--
--shorten :: Vec a n -> Vec a (Pred n)
--shorten xs = case null' xs of
--               Null    -> xs
--               NotNull -> tail' xs
--
--length' :: Vec a n -> Int
--length' Nil = 0
--length' (Cons _ tl) = 1 + length' tl

