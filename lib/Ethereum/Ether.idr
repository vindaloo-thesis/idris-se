module Ethereum.Ether

import Effects
import Ethereum.Types

------------ TYPES -----------------
data Commit a = Comm a

-------------- EFFECT --------------

data Ether : Nat -> Nat -> Nat -> Type where
  MkS : (value: Nat) -> (trans: Nat) -> (saved: Nat) -> Ether value trans saved

data EtherRules : Effect where
  Value   : sig EtherRules Nat
            (Ether v t s)
  Balance : Address -> sig EtherRules Nat
            (Ether v t s)
  Save    : (a : Nat) -> 
            sig EtherRules ()
            (Ether v t s)
            (Ether v t (s+a))
  Send    : (a : Nat) ->
            (r : Address) ->
            sig EtherRules ()
            (Ether v t s)
            (Ether v (t+a) s)

ETH : Nat -> Nat -> Nat -> EFFECT
ETH v t s = MkEff (Ether v t s) EtherRules

ETH_IN : Nat -> EFFECT
ETH_IN v = ETH v 0 0

ETH_OUT : Nat -> Nat -> Nat -> EFFECT
ETH_OUT = ETH

value : Eff Nat
       [ETH v t s]
value = call $ Value

balance : Address -> Eff Nat
       [ETH v t s]
balance a = call $ (Balance a)

save : (a : Nat) -> Eff ()
       [ETH v t s]
       [ETH v t (s+a)]
save a = call $ Save a

send : (a : Nat) -> (r : Address) -> Eff ()
       [ETH v t s]
       [ETH v (t+a) s]
send a r = call $ Send a r

