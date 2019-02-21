module Rea.Database where

import Rea.Types

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.IntMap as M


empty :: Database e
empty = M.empty


fromList :: [e] -> Database e
fromList es =
  M.fromList $ zip (iterate (+1) 0) es


-- | Perform the resource transfer over all appropriate
--   entities (for whom the predicate/relation holds).
--   Returns total number of transfers done.
performTransfer :: Eq e => Transfer env e -> Rea env e Int
performTransfer (Constant restr endo) = do
  db <- get
  env <- ask

  let ks = M.keys (M.filter (restr env) db)
  forM_
    ks
    (\k -> modify $ M.adjust (appEndo (endo env)) k)

  return $ length ks

performTransfer (Mutable restr endo) = do
  db <- get
  env <- ask

  let kes = M.keys db
  let rels = [(k1, k2)| k1 <- kes, k2 <- kes, k1 /= k2]
  forM_
    rels
    (\(k1, k2) -> do
        e1 <- gets (M.! k1)
        e2 <- gets (M.! k2)

        when (restr env e1 e2) $ do
          let (endo1, endo2) = endo env e1 e2
          let e1' = appEndo endo1 e1
          modify $ M.adjust (const e1') k1
          modify $ M.adjust (appEndo endo2) k2
    )

  return $ length rels

performTransfer (ThresholdConstant restr endo tp tendo) =
  do
    db <- get
    env <- ask

    let ks = M.keys (M.filter (restr env) db)
    forM_
      ks
      (\k -> do
          modify $ M.adjust (appEndo (endo env)) k

          e <- gets (M.! k)
          when (tp e) $
            modify $ M.adjust (appEndo (tendo env)) k
      )

    return $ length ks

performTransfer (ThresholdMutable restr endo tp tendo) =
  do
  db <- get
  env <- ask

  let kes = M.keys db
  let rels = [(k1, k2)| k1 <- kes, k2 <- kes, k1 /= k2]
  forM_
    rels
    (\(k1, k2) -> do
        e1 <- gets (M.! k1)
        e2 <- gets (M.! k2)

        when (restr env e1 e2) $ do
          let (endo1, endo2) = endo env e1 e2
          let e1' = appEndo endo1 e1
          modify $ M.adjust (const e1') k1
          modify $ M.adjust (appEndo endo2) k2

          --
          -- Threshold only in the first (left) entity.
          --
          when (tp e1') $
            modify $ M.adjust (appEndo (tendo env)) k1
    )

  return $ length rels


sequenceTransfers :: Eq e
                 => [Transfer env e] -> Rea env e ()
sequenceTransfers ts =
  mapM_ performTransfer ts
