module FreeMonad.Example where

import Control.Monad.Free ( Free(..) )

data Toy b next = Output b next
                | Bell next
                | Done
                deriving (Show)

{-
newtype Fix f = Fix (f (Fix f))

data FixE f e = Fix (f (FixE f e))
              | Throw e

catch :: (Functor f) => FixE f e1 -> (e1 -> FixE f e2) -> FixE f e2
catch (Fix x)   f = Fix $ fmap (`catch` f) x
catch (Throw e) f = f e
-}

instance Functor (Toy b) where
    fmap f (Output b next) = Output b $ f next
    fmap f (Bell next)     = Bell $ f next
    fmap _ Done            = Done

{-
data IncompleteException = IncompleteException

output :: a -> Free (Toy a) ()
output x = Free . Output x $ Pure ()

bell :: Free (Toy a) ()
bell = Free . Bell $ Pure ()

done :: Free (Toy a) r
done = Free Done
-}

liftF :: (Functor f) => f r -> Free f r
liftF command = Free $ fmap Pure command

output :: a -> Free (Toy a) ()
output x = liftF $ Output x ()

bell :: Free (Toy a) ()
bell = liftF $ Bell ()

done :: Free (Toy a) r
done = liftF Done

subroutine :: Free (Toy Char) ()
subroutine = output 'A'

program :: Free (Toy Char) r
program = do
    subroutine
    bell
    done

showProgram :: (Show a, Show r) => Free (Toy a) r -> String
showProgram (Free (Output a x)) = "output " ++ show a ++ "\n" ++ showProgram x
showProgram (Free (Bell x))     = "bell\n" ++ showProgram x
showProgram (Free Done)         = "done\n"
showProgram (Pure r)            = "return " ++ show r ++ "\n"

pretty :: (Show a, Show r) => Free (Toy a) r -> IO ()
pretty = putStr . showProgram
