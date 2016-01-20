module Ethereum.Ether

import Effects
import Ethereum.Types

------------ TYPES -----------------
data Commit a = Comm a

-------------- EFFECT --------------

data Ether : Nat -> Nat -> Nat -> Nat -> Type where
  MkS : (value: Nat) -> (balance: Nat) -> (trans: Nat) -> (saved: Nat) -> Ether value balance trans saved

data EtherRules : Effect where
  Value   : sig EtherRules Nat
            (Ether v b t s)
  Balance : Address -> sig EtherRules Nat
            (Ether v b t s)
  ContractBalance : sig EtherRules Nat
            (Ether v b t s)
  Save    : (a : Nat) -> 
            sig EtherRules ()
            (Ether v b t s)
            (Ether v b t (s+a))
  Send    : (a : Nat) ->
            (r : Address) ->
            sig EtherRules ()
            (Ether v b t s)
            (Ether v b (t+a) s)

ETH : Nat -> Nat -> Nat -> Nat -> EFFECT
ETH v b t s = MkEff (Ether v b t s) EtherRules

ETH_IN : Nat -> Nat -> EFFECT
ETH_IN v b = ETH v b 0 0

ETH_OUT : Nat -> Nat -> Nat -> Nat -> EFFECT
ETH_OUT = ETH

value : Eff Nat
       [ETH v b t s]
value = call $ Value

balance : Address -> Eff Nat
       [ETH v b t s]
balance a = call $ (Balance a)

contractBalance : Eff Nat
       [ETH v b t s]
contractBalance = call $ ContractBalance

save : (a : Nat) -> Eff ()
       [ETH v b t s]
       [ETH v b t (s+a)]
save a = call $ Save a

send : (a : Nat) -> (r : Address) -> Eff ()
       [ETH v b t s]
       [ETH v b (t+a) s]
send a r = call $ Send a r

