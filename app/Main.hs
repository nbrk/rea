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
db = fromList
  [
    Unit "unitA" (V2 0 0) [(V2 10 10)] 5 0 0
  , Unit "unitB" (V2 10 10) [] 5 0 0
  , Unit "unitC" (V2 0 0) [] 5 0 0
  , Unit "unitD" (V2 1 1) [] 5 0 0
  ]


logic :: Rea Env Unit ()
logic = do
  sequenceTransfers [fire, reload, move, updateWps]


main :: IO ()
main = do
  dbref <- newIORef db
  forever $ do
    modifyIORef dbref (execRea logic env)

    d <- readIORef dbref
    print d

    threadDelay 1000000
