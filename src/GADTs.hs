{-#LANGUAGE GADTs, KindSignatures #-}

module GADTs where

-- a is a Phantom, there is no value of type a anywhere
-- our constructors are restricted, before we could make I "String"
data Expr a where
    I :: Int -> Expr Int
    B :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Mul :: Expr Int -> Expr Int -> Expr Int
    Eq :: Expr Int -> Expr Int -> Expr Bool


eval :: Expr a -> a
eval (I n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Eq e1 e2) = eval e1 == eval e2


data Lam :: * -> * where
  Lift :: a                     -> Lam a
  Tup  :: Lam a -> Lam b        -> Lam (a, b)
  Lam  :: (Lam a -> Lam b)      -> Lam (a -> b)
  App  :: Lam (a -> b) -> Lam a -> Lam b
  Fix  :: Lam (a -> a)          -> Lam a

evalL :: Lam t -> t
evalL (Lift v)    = v
evalL (Tup e1 e2) = (evalL e1, evalL e2)
evalL (Lam f)     = \x -> evalL (f (Lift x))
evalL (App e1 e2) = (evalL e1) (evalL e2)
evalL (Fix f)     = (evalL f) (evalL (Fix f))


demo :: IO ()
demo = do
    putStrLn "Hello"
    let example1 = (I 5 `Add` I 1) `Mul` I 7
        example2 = (I 5 `Add` I 1) `Eq` I 7
--        example3 = (I 5 `Eq` I 1) `Mul` I 7
        lamFact = Fix (Lam (\f -> Lam (\y -> Lift (if evalL y == 0 then 1 else evalL y * (evalL f) (evalL y - 1)))))
--        lamWrong = App (Lam (\x -> Lam (\y -> App x y))) (Lift True)
    print $ eval example1
    print $ eval example2
    print $ evalL $ App lamFact (Lift 5)

