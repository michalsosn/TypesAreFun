module MeThinks where

import Control.Applicative
import Control.Monad
import Debug.Trace

foo :: Int -> Int -> Int -> Int
foo x y z =
    let x3 = trace "x3" $ x * x * x
        y2 = trace "y2" $ y * y
    in  x3 + y2 + z

bar :: Int -> Int -> Int -> Int
bar x =
    let x3 = trace "x3" $ x * x * x
    in  \y ->
    let y2 = trace "y2" $ y * y
    in  \z -> x3 + y2 + z

appTest :: (Int -> Int -> Int -> Int) -> IO ()
appTest f = do
    let results = f <$> [2, 4] <*> [1, 3, 5] <*> [10, 20]
    forM_ results print

(?) :: Bool -> (a, a) -> a
True  ? (a, _) = a
False ? (_, a) = a

demo :: IO ()
demo = do
--    putStrLn "Testing foo"
--    appTest foo
--    putStrLn "Testing bar"
--    appTest bar
    putStrLn $ (1 == 1) ? ("rowne", "rozne")
