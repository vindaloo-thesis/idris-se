module Ethereum.Ether

import Effects
import Ethereum.Types

------------ TYPES -----------------
data Commit a = Comm a

-------------- EFFECT --------------
-- value transferred saved
data CState = Running Nat Nat Nat

Init : Nat -> CState
Init v = Running v 0 0

data Ether : CState -> Type where
  MkS : (value: Nat) -> (trans: Nat) -> (saved: Nat) -> Ether (Running value trans saved)

instance Default CState where
  default = Init 1

instance Default (Ether (Running v 0 0)) where
  default {v} = MkS v 0 0 

data EtherRules : Effect where
  Value   : sig EtherRules Nat
            (Ether (Running v t s))
  Balance : Address -> sig EtherRules Nat
            (Ether (Running v t s))
  Save    : (a : Nat) -> 
            sig EtherRules ()
            (Ether (Running v t s))
            (Ether (Running v t (s+a)))
  Send    : (a : Nat) ->
            (r : Address) ->
            sig EtherRules ()
            (Ether (Running v t s))
            (Ether (Running v (t+a) s))

ETH : CState -> EFFECT
ETH h = MkEff (Ether h) EtherRules

ETH_IN : Nat -> EFFECT
ETH_IN v = ETH (Init v)

ETH_OUT : Nat -> Nat -> Nat -> EFFECT
ETH_OUT v t s = ETH (Running v t s)

value : Eff Nat
       [ETH (Running v t s)]
value = call $ Value

balance : Address -> Eff Nat
       [ETH (Running v t s)]
balance a = call $ (Balance a)

save : (a : Nat) -> Eff ()
       [ETH (Running v t s)]
       [ETH (Running v t (s+a))]
save a = call $ Save a

send : (a : Nat) -> (r : Address) -> Eff ()
       [ETH (Running v t s)]
       [ETH (Running v (t+a) s)]
send a r = call $ Send a r

