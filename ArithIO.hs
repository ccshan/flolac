{-# OPTIONS -W #-}

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr
          | Input | Output Expr
  deriving (Eq, Show)

eval (Lit v)     = return v
eval (Add e1 e2) = eval e1 >>= \v1 -> eval e2 >>= \v2 -> return (v1 + v2)
eval (Mul e1 e2) = eval e1 >>= \v1 -> eval e2 >>= \v2 -> return (v1 * v2)

#ifdef SOLUTION
eval Input      = getLine >>= \str -> return (read str)
eval (Output e) = eval e >>= \v -> print v >>= \() -> return v
eval :: Expr -> IO Int

main = eval (Output (Add (Lit 2) (Lit 2)))
#endif
