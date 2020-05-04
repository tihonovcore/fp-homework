module Utils where

import Debug.Trace (trace)

mtrace :: (Monad m, Show a) => m a -> m a
mtrace m = m >>= (\con -> trace (show con) m)

strace :: (Show a) => a -> a
strace a = trace (show a) a
