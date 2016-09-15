module STMExample.Reminders
    ( exec
    ) where

import Control.Concurrent
    ( forkIO
    , threadDelay
    )
import Control.Monad
    ( unless
    )
import Text.Printf
    ( printf
    )

setReminder :: String -> IO ()
setReminder s = do
    let t = read s :: Int
    printf "OK, I'll remind you in %d seconds\n" t
    threadDelay $ 10 ^ 6 * t
    printf "%d seconds is up!\n" t

exec :: IO ()
exec = loop
  where
    loop = do
        s <- getLine
        unless (s == "exit") $ do
            forkIO $ setReminder s
            loop
