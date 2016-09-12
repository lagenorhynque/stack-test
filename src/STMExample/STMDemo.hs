module STMExample.STMDemo
    ( exec
    ) where

import Control.Concurrent.STM
    ( STM
    , TVar
    , atomically
    , modifyTVar
    , newTVar
    , readTVar
    , writeTVar
    )

n :: STM (TVar Int)
n = newTVar 0

f :: Int -> Int
f = (* 2)

exec :: Int -> IO Int
exec x = atomically $ do
    r <- n
    writeTVar r x
    modifyTVar r f
    readTVar r
