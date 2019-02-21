module Rea.Types where

import Control.Monad.Reader
import Control.Monad.State
import Data.Monoid
import qualified Data.IntMap as M


-- | The entity database
type Database e = M.IntMap e


-- | A typed transfer of resources
data Transfer env e
  = Constant
      (env -> e -> Bool)
      (env -> Endo e)
  | Mutable
      (env -> e -> e -> Bool)
      (env -> e -> e -> (Endo e, Endo e))
  | ThresholdConstant
      (env -> e -> Bool)
      (env -> Endo e)
      (e -> Bool)
      (env -> Endo e)
  | ThresholdMutable
      (env -> e -> e -> Bool)
      (env -> e -> e ->(Endo e, Endo e))
      (e -> Bool)
      (env -> Endo e)


type Rea env e = ReaderT env (State (Database e))
