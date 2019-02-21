{-# LANGUAGE Rank2Types #-}
module Rea.Transfer where

import Rea.Types

import Data.Monoid
import Data.Bifunctor
import Lens.Micro



-- | A predicate that always holds for the given entity
always :: env -> e -> Bool
always _ _ = True


-- | A predicate that never holds for the given entity
never :: e -> Bool
never = const False

-- | A threshold predicate that always holds
talways :: e -> Bool
talways = const True


-- | A threshold predicate that never holds
tnever :: env -> e -> Bool
tnever _ _ = False


-- | A relation that always holds between two entities
malways :: env -> e1 -> e2 -> Bool
malways _ _ _ = True


-- | A relation that never holds between two entities
mnever :: env -> e1 -> e2 -> Bool
mnever _ _ _ = False


-- | Constant resource transfer
cx :: (env -> e -> Bool)
      -> ASetter' e r
      -> (env -> e -> r)
      -> Transfer env e
cx pred sel rf =
  let ef = \env e -> set sel (rf env e) e
  in
    Constant pred (Endo . ef)


-- | Mutable resource transfer (exchange)
mx :: (env -> e -> e -> Bool)
                -> ASetter' e rsrc
                -> ASetter' e rdst
                -> (env -> e -> e -> (rsrc, rdst))
                -> Transfer env e
mx rel srcsel dstsel rf =
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
tcx :: (env -> e -> Bool)
       -> Lens' e r1
       -> (env -> e -> r1)
       -> (r1 -> Bool)
       -> ASetter' e r2
       -> (env -> e -> r2)
       -> Transfer env e
tcx pred sel rf tpred tsel trf =
  let ef = \env e -> e & sel .~ (rf env e)
      tef = \env e -> e & tsel .~ (trf env e)
  in
    ThresholdConstant
    pred
    (Endo . ef)
    (\e -> tpred (e ^. sel))
    (Endo . tef)


-- | Mutable resource transfer with a threshold on first
tmx :: (env -> e -> e -> Bool)
       -> Lens' e rsrc
       -> ASetter' e rdst
       -> (env -> e -> e -> (rsrc, rdst))
       -> (rsrc -> Bool)
       -> ASetter' e r
       -> (env -> e -> r)
       -> Transfer env e
tmx rel srcsel dstsel rf tpred tsel trf =
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

