module STMExample.NonSTMDemo2
    ( exec
    ) where

import Data.IORef
    ( IORef
    , modifyIORef
    , newIORef
    , readIORef
    , writeIORef
    )

n :: IO (IORef Int)
n = newIORef 0

f :: Int -> Int
f = (* 2)

exec :: Int -> IO Int
exec x = do
    r <- n
    writeIORef r x
    modifyIORef r f
    readIORef r
