module STMExample.NonSTMDemo
    ( exec
    ) where

import Control.Concurrent
    ( MVar
    , forkIO
    , modifyMVar_
    , newMVar
    , putMVar
    , readMVar
    , takeMVar
    )

n :: IO (MVar Int)
n = newMVar 0

f :: Int -> Int
f = (* 2)

exec :: Int -> IO Int
exec x = do
    r <- n
    takeMVar r
    putMVar r x
    modifyMVar_ r $ return . f
    readMVar r
