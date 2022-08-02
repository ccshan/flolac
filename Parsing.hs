{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheckAll)
import Control.Monad (liftM, ap)
import Data.Foldable (traverse_)
import Data.Char (ord, isDigit)
import Control.Monad.Trans.State (execState, modify)

newtype Parser a = MkParser {parse :: String -> [(a, String)]}

instance Functor Parser where fmap = liftM

instance Applicative Parser where pure = return; (<*>) = ap

instance Monad Parser where
  return a = MkParser (\s -> [(a, s)])
  m >>= k  = MkParser (\s -> concatMap (\(a, s') -> parse (k a) s')
                                       (parse m s))

amb :: [Parser a] -> Parser a
-- ^Parse something that satisfies one of the given parsers
amb ms = MkParser (\s -> concatMap (\m -> parse m s) ms)

item :: Parser Char
-- ^Parse the next character, as long as there is one
item = MkParser (\s -> case s of ""     -> []
                                 c:rest -> [(c,rest)])

end :: Parser ()
end = MkParser (\s -> case s of "" -> [((),s)]
                                _  -> [])

--------------------------------------------------------------------------------

sat :: (Char -> Bool) -> Parser Char
-- ^Parse the next character, which must satisfy the given predicate
sat p = do c <- item
           if p c then return c else amb []

digit :: Parser Int
digit = do c <- sat isDigit
           return (ord c - ord '0')

many :: Parser a -> Parser [a]
many m = amb [many1 m, return []]

many1 :: Parser a -> Parser [a]
many1 m = do a <- m
             as <- many m
             return (a:as)

number :: Parser Int
prop_number1 = parse number "123" == [(123,""), (12,"3"), (1,"23")]
prop_number2 = parse number "12a" == [(12,"a"), (1,"2a")]
prop_number3 = parse number "abc" == []
number = do digits <- many1 digit
            return (execState (traverse_ (\n -> modify (\s -> s * 10 + n)) digits) 0)

#if STEP == 1
data Tree = Leaf Int | Branch Tree Tree
  deriving (Eq, Show)

tree :: Parser Tree
prop_tree1 = parse tree "123" == [(Leaf 123, ""), (Leaf 12, "3"), (Leaf 1, "23")]
prop_tree2 = parse tree "<12,34>" == [(Branch (Leaf 12) (Leaf 34), "")]
prop_tree3 = parse tree "<12,<34,567>>" == [(Branch (Leaf 12) (Branch (Leaf 34) (Leaf 567)), "")]
prop_tree4 = parse tree "<<12,34>,567>" == [(Branch (Branch (Leaf 12) (Leaf 34)) (Leaf 567), "")]
prop_tree5 = parse tree "<<1,2>,<3,4>>" == [(Branch (Branch (Leaf 1) (Leaf 2))
                                                    (Branch (Leaf 3) (Leaf 4)), "")]
tree = amb [do n <- number
               return (Leaf n),
#ifdef SOLUTION
            do sat ('<' ==)
               t1 <- tree
               sat (',' ==)
               t2 <- tree
               sat ('>' ==)
               return (Branch t1 t2)]
#else
            _]
#endif
#endif

#if STEP > 1
char :: Char -> Parser Char
char c = sat (c ==)
#endif

#if STEP == 2
sepby1 :: Parser a -> Parser b -> Parser [a]
prop_sepby1 = parse (sepby1 number (char '+') <* end)
                    "123+45+67"
              == [([123,45,67], "")]
prop_sepby2 = parse (sepby1 (amb [char '+', char '-']) number <* end)
                    "+123-45+67+"
              == [(['+','-','+','+'], "")]
#ifdef SOLUTION
sepby1 m sep = do a <- m
                  as <- many (sep >> m)
                  return (a:as)
#endif
#endif

#if STEP == 3
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 m op = m >>= rest
  where rest a = amb [do f <- op
                         b <- m
                         rest (f a b),
                      return a]

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr
  deriving (Eq, Show)

expr :: Parser Expr
prop_expr1 = parse (expr <* end) "12+34*567+89"
             == [(Add (Add (Lit 12) (Mul (Lit 34) (Lit 567))) (Lit 89), "")]
prop_expr2 = parse (expr <* end) "(12+34)*567+89"
             == [(Add (Mul (Add (Lit 12) (Lit 34)) (Lit 567)) (Lit 89), "")]
prop_expr3 = parse (expr <* end) "((12*34))"
             == [(Mul (Lit 12) (Lit 34), "")]
prop_expr4 = parse (expr <* end) "12-34*567+89"
             == [(Add (Add (Lit 12) (Mul (Lit (-1)) (Mul (Lit 34) (Lit 567))))
                      (Lit 89),
                  "")]
prop_expr5 = parse (expr <* end) "12-34*567-89"
             == [(Add (Add (Lit 12) (Mul (Lit (-1)) (Mul (Lit 34) (Lit 567))))
                      (Mul (Lit (-1)) (Lit 89)),
                  "")]
#ifdef SOLUTION
expr = chainl1 term
               (amb [char '+' >> return Add,
                     char '-' >> return (\e1 e2 -> Add e1 (Mul (Lit (-1)) e2))])
#else
expr = chainl1 term (char '+' >> return Add)
#endif

term :: Parser Expr
prop_term1 = parse (term <* end) "12+34*567+89" == []
prop_term2 = parse (term <* end) "(12+34)*567+89" == []
prop_term3 = parse (term <* end) "(12+34)*567"
             == [(Mul (Add (Lit 12) (Lit 34)) (Lit 567), "")]
term = chainl1 factor (char '*' >> return Mul)

factor :: Parser Expr
factor = amb [do v <- number
                 return (Lit v),
              do char '('
                 e <- expr
                 char ')'
                 return e]
#endif

return []
main = $quickCheckAll >>= print
