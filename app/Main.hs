module Main where

import Lib
    ( someFunc
    )
import qualified STMExample.NonSTMDemo as N
import qualified STMExample.STMDemo as S

main :: IO ()
main = do
    someFunc
    r <- N.exec 3
    print r
    s <- S.exec 3
    print s
