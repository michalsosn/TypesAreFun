{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Continuations where

import Control.Applicative
import Control.Monad

add_cps :: Int -> Int -> (Int -> r) -> r
add_cps x y f = f (x + y)

square_cps :: Int -> (Int -> r) -> r
square_cps x f = f (x * x)

pythagoras_cps :: Int -> Int -> (Int -> r) -> r
pythagoras_cps x y k =
    square_cps x $ \x_squared ->
    square_cps y $ \y_squared ->
    add_cps x_squared y_squared k

thrice :: (a -> a) -> a -> a
thrice f x = f (f (f x))

thrice_cps :: (a -> (a -> r) -> r) -> a -> (a -> r) -> r
thrice_cps f x k =
    f x  $ \x1 ->
    f x1 $ \x2 ->
    f x2 k

chainCPS :: ((a -> r) -> r) -> (a -> (b -> r) -> r) -> (b -> r) -> r
chainCPS f g k = f $ \x -> g x k

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Functor (Cont r) where
    fmap f x = Cont $ \k -> runCont x (k . f)

instance Applicative (Cont r) where
    pure x = Cont ($x)
    x <*> y = Cont $ \k -> runCont x $ \ab -> runCont y $ \a -> k (ab a)

instance Monad (Cont r) where
    return x = Cont ($x)
    x >>= f = Cont $ \k -> runCont x $ \a -> runCont (f a) k

add_cont :: Int -> Int -> Cont r Int
add_cont x y = return (x + y)

square_cont :: Int -> Cont r Int
square_cont x = return (x * x)

pythagoras_cont :: Int -> Int -> Cont r Int
pythagoras_cont x y = do
    x_squared <- square_cont x
    y_squared <- square_cont y
    add_cont x_squared y_squared

callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = Cont $ \h -> runCont (f (\a -> Cont $ \_ -> h a)) h

foo :: Int -> Cont r String
foo x = callCC $ \k -> do
    let y = x ^ 2 + 3
    when (y > 30) $ k "over thirty"
    when (y > 20) $ k "over twenty"
    when (y > 10) $ k "over ten"
    return (show $ y - 4)

demo :: IO ()
demo = do
    pythagoras_cps 3 4 print
    print $ thrice (+3) 10
    thrice_cps (add_cps 3) 10 print
    runCont (pythagoras_cont 3 4) print
    runCont (foo 10) putStrLn
    runCont (foo 30) putStrLn

