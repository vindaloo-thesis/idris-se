module Ethereum.Environment

import Effects
import Ethereum.Types

-------------- EFFECT --------------
--  contractAddress sender origin remaningGas
data EState = CEnv Address Address Address

data Env : EState -> Type where
  MkE : (cAddr: Address)  ->
        (sender: Address) ->
        (origin: Address) ->
        Env (CEnv cAddr sender origin)

data EnvRules : Effect where
  ContractAddress : sig EnvRules Address (Env e)
  Sender          : sig EnvRules Address (Env e)
  Origin          : sig EnvRules Address (Env e)
  RemainingGas    : sig EnvRules Nat (Env e)
  TimeStamp       : sig EnvRules Nat (Env e)
  Coinbase        : sig EnvRules Address (Env e)

ENV : EState -> EFFECT
ENV e = MkEff (Env e) EnvRules

contractAddress : Eff Address [ENV e]
contractAddress = call $ ContractAddress

sender : Eff Address [ENV e]
sender = call $ Sender

origin : Eff Address [ENV e]
origin = call $ Origin

remainingGas : Eff Nat [ENV e]
remainingGas = call $ RemainingGas

timeStamp : Eff Nat [ENV e]
timeStamp = call $ TimeStamp

coinbase : Eff Address [ENV e]
coinbase = call $ Coinbase


