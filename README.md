# rea

## Description
High-performance, purely functional, minimalistic DSL
implementation of the Resource-Entity-Action pattern. The 
pattern was originally conceived as a generalization of Real Time Strategy games, but the abstraction suits
also well for other types of discrete simulations.

Based on the ideas proposed in research paper `Resource Entity Action: A Generalized Design Pattern for RTS games`, by Abbadi, Mohamed & Di Giacomo and others.

[https://www.researchgate.net/publication/259182804_Resource_Entity_Action_A_Generalized_Design_Pattern_for_RTS_games](https://www.researchgate.net/publication/259182804_Resource_Entity_Action_A_Generalized_Design_Pattern_for_RTS_games)


## Idea
The concept presented in the original paper centers 
around transfers of some kinds of abstract energies between objects (i.e. units). These transfers (also called 
actions) can be either intransitive (affecting only just one object by a constant) or transitive (where two objects exchanges energies). 
In  addition there are "thresholds" that can be embedded in
the transfer to automatically check for some special conditions (i.e. a building of a base has been completed 100%, a
resource goes below required minimum, etc.) after the transfer is made. Authors claim in the paper that by using only
these 4 actions it is possible to describe every RTS game.

## Implementation
I have implemented four combinators: `cx`, `mx`, `tcx`, `tmx` which correspond to these **c**onstant, **m**utual, **t**hreshold-**c**onstant and **t**hreshold-**m**utual actions. You generate [lenses](http://hackage.haskell.org/package/microlens-th) for your datatype and use them to construct
the desired transfers in a type-safe manner. You then 
execute them in the `Rea` monad, which is just `Reader` of the environment and `State` of the immutable database of objects (units, etc).

For example, here's how simple a complete move-fire-reload
logic could be described with `era` (notice heavy usage of `lens`):

``` haskell
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

```

We then assemble these transfers inside the `Rea` monad 
and run the computation using `runRea`:

``` haskell
logic :: Rea Env Unit ()
logic = do
  sequenceTransfers [fire, reload, move, updateWps]
```

