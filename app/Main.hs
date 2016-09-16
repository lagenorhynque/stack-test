module Main where

import Lib
    ( someFunc
    )
import qualified STMExample.NonSTMDemo as N
import qualified STMExample.NonSTMDemo2 as N2
import qualified STMExample.STMDemo as S

main :: IO ()
main = do
    someFunc
    r <- N.exec 3
    print r
    r2 <- N2.exec 3
    print r2
    s <- S.exec 3
    print s
