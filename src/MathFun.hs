module MathFun where

fib1 :: [Integer]
fib1 = zipWith (+) (0:fib1) (0:1:fib1)

fib2 :: [Integer]
fib2 = 0:1:zipWith (+) fib2 (tail fib2)

unique :: Eq a => [a] -> [a]
unique (a:b:r) = if a == b then unique (b:r) else a:unique (b:r)
unique r = r

demo :: IO ()
demo = do
    print $ take 10 fib1
    print $ take 10 fib2
    let x = [1, 2, 1, 1, 2, 3, 2, 3, 3, 4]
    print $ unique x


