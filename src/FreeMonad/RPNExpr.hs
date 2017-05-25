module FreeMonad.RPNExpr where

import Control.Monad ( foldM )
import Control.Monad.Free ( Free(..) )

data RPNExpr n expr = Number n expr
                    | Add expr
                    | Sub expr
                    | Mul expr
                    | End
                    deriving (Show)

instance Functor (RPNExpr n) where
    fmap f (Number n expr) = Number n $ f expr
    fmap f (Add expr)      = Add $ f expr
    fmap f (Sub expr)      = Sub $ f expr
    fmap f (Mul expr)      = Mul $ f expr
    fmap _ End             = End

liftF :: (Functor f) => f r -> Free f r
liftF = Free . fmap Pure

type RPN a b = Free (RPNExpr a) b

num :: a -> RPN a ()
num n = liftF $ Number n ()

add :: RPN a ()
add = liftF $ Add ()

sub :: RPN a ()
sub = liftF $ Sub ()

mul :: RPN a ()
mul = liftF $ Mul ()

end :: RPN a b
end = liftF End

stringify :: (Show a) => RPN a b -> String
stringify (Free (Number n e)) = show n ++ " " ++ stringify e
stringify (Free (Add e))      = "+ " ++ stringify e
stringify (Free (Sub e))      = "- " ++ stringify e
stringify (Free (Mul e))      = "* " ++ stringify e
stringify (Free End)          = "."
stringify (Pure _)            = ""

parse :: (Read a) => String -> Either String (RPN a ())
parse = foldM rpn (Pure ()) . reverse . words
  where
    rpn e "+" = Right . Free $ Add e
    rpn e "-" = Right . Free $ Sub e
    rpn e "*" = Right . Free $ Mul e
    rpn _ "." = Right $ Free End
    rpn e n   = case reads n of
        [(v,_)] -> Right . Free $ Number v e
        _       -> Left "invalid input"

eval :: (Num a) => RPN a b -> Either String a
eval = calc []
  where
    calc stack      (Free (Number n e)) = calc (n : stack) e
    calc (n1:n2:ns) (Free (Add e))      = calc (n2 + n1 : ns) e
    calc (n1:n2:ns) (Free (Sub e))      = calc (n2 - n1 : ns) e
    calc (n1:n2:ns) (Free (Mul e))      = calc (n2 * n1 : ns) e
    calc (n:_)      (Free End)          = Right n
    calc (n:_)      (Pure _)            = Right n
    calc _          _                   = Left "invalid expression"

-- example
expr1 :: RPN Double ()
expr1 = do
    num 8
    num 6
    num 1
    sub
    mul

expr2 :: RPN Double ()
expr2 = do
    num 2
    add
    end

value :: Either String Double
value = do
    expr <- parse . stringify $ expr1 >> expr2
    eval expr
