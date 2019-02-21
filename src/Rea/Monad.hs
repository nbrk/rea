module Rea.Monad where

import Rea.Types

import Control.Monad.Reader
import Control.Monad.State


-- instance Functor (Rea env) where
--   fmap dbf r =
--     let db = reaDatabase r
--     in
--       r { reaDatabase = dbf db }


-- | Run a Rea computation (i.e. a game turn)
runRea :: Rea env e a -> env -> Database e
       -> (a, Database e)
runRea r env db =
   runState (runReaderT r env) db
