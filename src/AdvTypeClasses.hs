{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}

module AdvTypeClasses where

class Eq e => Collection c e | c -> e  where
    insert :: c -> e -> c
    member :: c -> e -> Bool

instance Eq a => Collection [a] a where
    insert = flip (:)
    member = flip elem

demo :: IO ()
demo = do
    let x = [1, 2, 3]
        y = insert x 5
    print y
