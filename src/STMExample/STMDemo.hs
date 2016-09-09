module STMExample.STMDemo
    ( exec
    ) where

import Control.Concurrent.STM
    ( STM
    , TVar
    , atomically
    , newTVar
    , readTVar
    )

f :: Int -> STM (TVar Int)
f x = newTVar $ x * 2

exec :: Int -> IO Int
exec x = atomically $ do
    r <- f x
    readTVar r
