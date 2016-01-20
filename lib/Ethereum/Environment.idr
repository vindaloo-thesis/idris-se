module Ethereum.Environment

import Effects
import Ethereum.Types

-------------- EFFECT --------------

data Env : Address -> Address -> Address -> Type where
  MkE : (cAddr: Address)  ->
        (sender: Address) ->
        (origin: Address) ->
        Env cAddr sender origin

data EnvRules : Effect where
  ContractAddress : sig EnvRules Address (Env c s o)
  Sender          : sig EnvRules Address (Env c s o)
  Origin          : sig EnvRules Address (Env c s o)
  RemainingGas    : sig EnvRules Nat (Env c s o)
  TimeStamp       : sig EnvRules Nat (Env c s o)
  Coinbase        : sig EnvRules Address (Env c s o)

ENV : Address -> Address -> Address -> EFFECT
ENV c s o = MkEff (Env c s o) EnvRules

contractAddress : Eff Address [ENV c s o]
contractAddress = call $ ContractAddress

sender : Eff Address [ENV c s o]
sender = call $ Sender

origin : Eff Address [ENV c s o]
origin = call $ Origin

remainingGas : Eff Nat [ENV c s o]
remainingGas = call $ RemainingGas

timeStamp : Eff Nat [ENV c s o]
timeStamp = call $ TimeStamp

coinbase : Eff Address [ENV c s o]
coinbase = call $ Coinbase


