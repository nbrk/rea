{-# LANGUAGE TemplateHaskell #-}
module Main where

import Rea

import Control.Monad
import Control.Concurrent
import Data.IORef
import Data.Maybe
import Linear
import Lens.Micro
import Lens.Micro.TH
import SDL
import Criterion.Main


type Env = ()
data Unit
  = Unit
  { _unitName :: String
  , _unitPos :: V2 Double
  , _unitWps :: [V2 Double]
  , _unitAttack :: Int
  , _unitReloading :: Int
  , _unitDamage :: Int
  }
  deriving (Show, Eq)

makeLenses ''Unit



move :: Transfer Env Unit
move =
  cx
    (\_ u -> not (u ^. unitWps . to null))
    unitPos
    (\_ u ->
       let wp = u ^. unitWps . to head
           pos = u ^. unitPos
           d = distance pos wp
           v = normalize $ wp ^-^ pos
       in
         if d <= 1 then wp else u ^. unitPos + v
    )


updateWps :: Transfer Env Unit
updateWps =
  cx
    (\_ u -> let mbwp = u ^. unitWps . to listToMaybe
             in
               case mbwp of
                 Nothing -> False
                 Just wp -> u ^. unitPos == wp
    )
    unitWps
    (\_ u -> u ^. unitWps . to tail)



fire :: Transfer Env Unit
fire =
  tmx
    (\_ u1 u2 ->
       distance (u1 ^. unitPos) (u2 ^. unitPos) < 3
       && u1 ^. unitAttack > 0
       && u1 ^. unitReloading == 0
    )
    unitAttack
    unitDamage
    (\_ u1 u2 ->
       (u1 ^. unitAttack - 1, u2 ^. unitDamage + 1)
    )
    talways
    unitReloading
    (\_ _ -> 3)


reload :: Transfer Env Unit
reload =
  cx
    (\_ u -> u ^. unitReloading > 0)
    unitReloading
    (\_ u -> u ^. unitReloading - 1)



env = ()
-- db = fromList
--   [
--     Unit "unitA" (V2 100 100) [(V2 10 10)] 5 0 0
--   , Unit "unitB" (V2 200 200) [(V2 10 10)] 5 0 0
--   , Unit "unitC" (V2 400 400) [(V2 10 10)] 5 0 0
--   , Unit "unitD" (V2 1 1) [(V2 100 100)] 5 0 0
--   , Unit "unitA" (V2 150 100) [(V2 40 40)] 5 0 0
--   , Unit "unitB" (V2 250 200) [(V2 50 50)] 5 0 0
--   , Unit "unitC" (V2 400 440) [(V2 60 50)] 5 0 0
--   , Unit "unitD" (V2 10 10) [(V2 100 100)] 5 0 0
--   , Unit "unitB" (V2 200 240) [(V2 70 90)] 5 0 0
--   , Unit "unitC" (V2 400 440) [(V2 70 30)] 5 0 0
--   , Unit "unitD" (V2 30 1) [(V2 100 100)] 5 0 0
--   , Unit "unitA" (V2 140 100) [(V2 70 40)] 5 0 0
--   , Unit "unitB" (V2 240 200) [(V2 110 10)] 5 0 0
--   , Unit "unitC" (V2 440 400) [(V2 10 10)] 5 0 0
--   , Unit "unitD" (V2 100 1) [(V2 100 100)] 5 0 0
--   ]


logic :: Rea Env Unit ()
logic = do
  sequenceTransfers [fire, reload, move, updateWps]

---------

frame :: IORef (Database Unit) -> Renderer -> TimerCallback
frame dbref ren _ = do
  us <- return . toList =<< readIORef dbref
  let posv =
        fmap
          (\u -> (P (fmap truncate (u ^. unitPos))))
          us

  rendererDrawColor ren $= V4 0 0 0 255
  clear ren

  rendererDrawColor ren $= V4 255 255 255 255
  forM_
    posv
--    (\p -> drawRect ren (Just (Rectangle p (V2 10 10))))
    (drawPoint ren)

  present ren
  modifyIORef dbref (execRea logic Main.env)
  return $ Reschedule 41 -- 16


-- turn :: IORef (Database Unit) -> Rea Env e () -> Env -> TimerCallback
-- turn dbref r env _ = do
--   modifyIORef dbref (execRea logic env)
--   return $ Reschedule 100



main :: IO ()
main = do
--   dbref <- newIORef db
--   initializeAll
--   window <- createWindow mempty defaultWindow
--   renderer <- createRenderer window (-1) defaultRenderer

--   addTimer 41 (frame dbref renderer)
-- --  addTimer 100 (turn dbref logic env)

--   forever $ do
--     _ <- pollEvents
--     return ()

  let us = replicate 1000 $ Unit "unitA" (V2 100 100) [(V2 10 10)] 5 0 0
  dbref <- newIORef (fromList us)

  let io = modifyIORef' dbref (execRea logic Main.env)

  defaultMain [bench "1000" (nfIO io)]
--  forever $ do
      -- modifyIORef' dbref (execRea logic Main.env)
--      d <- readIORef' dbref
--      print d

--      threadDelay 1000000
