{-# LANGUAGE TypeFamilies #-}

module SemiFun where


fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)


type family Swap a where
    Swap Int = Bool
    Swap Bool = Int


recover :: Maybe Int -> Maybe Int
recover Nothing = Just 3
recover x@(Just _) = x


demo = do
    print $ fmap fib [1..10]
    print $ fmap recover [Nothing, Just 10, Just 5]
