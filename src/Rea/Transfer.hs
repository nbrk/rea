{-# LANGUAGE Rank2Types #-}
module Rea.Transfer where

import Rea.Types

import Data.Monoid
import Data.Bifunctor
import Lens.Micro



-- | A predicate that always holds for the given entity
predicateAlways :: env -> e -> Bool
predicateAlways _ _ = True


-- | A predicate that never holds for the given entity
predicateNever :: env -> e -> Bool
predicateNever _ _ = False


-- | A relation that always holds between two entities
relationAlways :: env -> e1 -> e2 -> Bool
relationAlways _ _ _ = True


-- | A relation that never holds between two entities
relationNever :: env -> e1 -> e2 -> Bool
relationNever _ _ _ = False



-- | The way of combining transfers
instance Semigroup (Transfer env e) where
  (<>) t1 t2 =
      let endo = toEndo t1 <> toEndo t2
      in
        mxfer
          relationAlways
          id
          id
          (\env e1 e2-> appEndo (endo env) (e1, e2))

-- | The no-op identity transfer
instance Monoid (Transfer env e) where
  mempty =
    mxfer relationNever id id (\env e1 e2-> (e1, e2))



-- | Constant resource transfer
cxfer :: (env -> e -> Bool)
      -> ASetter' e r
      -> (env -> e -> r)
      -> Transfer env e
cxfer pred sel rf =
  let ef = \env e -> set sel (rf env e) e
  in
    Constant pred (Endo . ef)


-- | Mutable resource transfer (exchange)
mxfer :: (env -> e -> e -> Bool)
                -> ASetter' e rsrc
                -> ASetter' e rdst
                -> (env -> e -> e -> (rsrc, rdst))
                -> Transfer env e
mxfer rel srcsel dstsel rf =
  let eef = \env esrc edst ->
        let (rsrc, rdst) = rf env esrc edst
        in
          ( (\esrc -> set srcsel rsrc esrc)
          , (\edst -> set dstsel rdst edst))
  in
    Mutable
    rel
    (\env e1 e2 -> bimap Endo Endo $ eef env e1 e2)


-- | Constant resource transfer with a threshold transfer
tcxfer :: (env -> e -> Bool)
       -> Lens' e r1
       -> (env -> e -> r1)
       -> (r1 -> Bool)
       -> ASetter' e r2
       -> (env -> e -> r2)
       -> Transfer env e
tcxfer pred sel rf tpred tsel trf =
  let ef = \env e -> e & sel .~ (rf env e)
      tef = \env e -> e & tsel .~ (trf env e)
  in
    ThresholdConstant
    pred
    (Endo . ef)
    (\e -> tpred (e ^. sel))
    (Endo . tef)


-- | Mutable resource transfer with a threshold on first
tmxfer :: (env -> e -> e -> Bool)
       -> Lens' e rsrc
       -> ASetter' e rdst
       -> (env -> e -> e -> (rsrc, rdst))
       -> (rsrc -> Bool)
       -> ASetter' e r
       -> (env -> e -> r)
       -> Transfer env e
tmxfer rel srcsel dstsel rf tpred tsel trf =
  let eef = \env esrc edst ->
              let (rsrc, rdst) = rf env esrc edst
              in
                ( (\esrc -> set srcsel rsrc esrc)
                , (\edst -> set dstsel rdst edst))
      tef = \env e -> e & tsel .~ (trf env e)
  in
    ThresholdMutable
    rel
    (\env e1 e2 -> bimap Endo Endo $ eef env e1 e2)
    (\e1 -> tpred (e1 ^. srcsel))
    (Endo . tef)


-- | Convert the action to an endomorphism of pairs
toEndo :: Transfer env e -> env -> Endo (e, e)
toEndo (Constant p f) env =
  Endo $ \asis@(e1, _e2) ->
           if p env e1
           then
             let endo1 = f env
             in
               (appEndo endo1 e1, _e2)
           else
             asis
toEndo (Mutable p f) env =
  Endo $ \asis@(e1, e2) ->
           if p env e1 e2
           then
             let (endo1, endo2) = f env e1 e2
             in
               (appEndo endo1 e1, appEndo endo2 e2)
           else
             asis
toEndo (ThresholdConstant p f tp tf) env =
  Endo $ \asis@(e1, _e2) ->
           if p env e1
           then
             let e1' = appEndo (f env) e1
                 e1'' = if tp e1'
                        then appEndo (tf env) e1'
                        else e1'
             in
               (e1'', _e2)
           else
             asis
toEndo (ThresholdMutable p f tp tf) env =
  Endo $ \asis@(e1, e2) ->
           if p env e1 e2
           then
             let (endo1, endo2) = f env e1 e2
                 (e1', e2') =
                   (appEndo endo1 e1, appEndo endo2 e2)
                 e1'' = if tp e1'
                        then appEndo (tf env) e1'
                        else e1'
             in
               (e1'', e2')
           else
             asis

