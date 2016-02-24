module Ethereum.Environment

import Effects
import Ethereum.Types

-------------- EFFECT --------------
data Env : Address -> Address -> Type where
  MkEnv : (sender : Address) -> -- Sender of transaction (current call)
          (origin : Address) -> -- Origin of transaction (full call chain)
          Env sender origin

data EnvRules : Effect where
  Self            : sig EnvRules Address (Ethereum.Environment.Env s o)
  Sender          : sig EnvRules Address (Ethereum.Environment.Env s o)
  Origin          : sig EnvRules Address (Ethereum.Environment.Env s o)
  RemainingGas    : sig EnvRules Nat (Ethereum.Environment.Env s o)
  TimeStamp       : sig EnvRules Nat (Ethereum.Environment.Env s o)
  Coinbase        : sig EnvRules Address (Ethereum.Environment.Env s o)

ENV : Address -> Address -> EFFECT
ENV s o = MkEff (Env s o) EnvRules

self : Eff Address [ENV s o]
self = call $ Self

sender : Eff Address [ENV s o]
sender = call $ Sender

origin : Eff Address [ENV s o]
origin = call $ Origin

remainingGas : Eff Nat [ENV s o]
remainingGas = call $ RemainingGas

timeStamp : Eff Nat [ENV s o]
timeStamp = call $ TimeStamp

coinbase : Eff Address [ENV s o]
coinbase = call $ Coinbase