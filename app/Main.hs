module Main where

import Lib
    ( someFunc
    )
import qualified STMExample.STMDemo as S

main :: IO ()
main = do
    someFunc
    r <- S.exec 3
    print r
