module Ethereum.Ether

import Effects
import Ethereum.Types

------------ TYPES -----------------
data Commit a = Comm a

-------------- EFFECT --------------

data Ether : Nat -> Nat -> Type where
  MkS : (value: Nat) -> (balance: Nat) -> Ether value balance

data EtherRules : Effect where
  Value   : sig EtherRules Nat
            (Ether v b)
  Balance : Address -> sig EtherRules Nat
            (Ether v b)
  ContractBalance : sig EtherRules Nat
            (Ether v b)
  Send    : (a : Nat) ->
            (r : Address) ->
            {auto p: LTE a b} ->
            sig EtherRules ()
            (Ether v b)
            (Ether v (b-a))

ETH : Nat -> Nat -> EFFECT
ETH v b = MkEff (Ether v b) EtherRules

value : Eff Nat
       [ETH v b]
value = call $ Value

balance : Address -> Eff Nat
       [ETH v b]
balance a = call $ (Balance a)

contractBalance : Eff Nat
       [ETH v b]
contractBalance = call $ ContractBalance

send : (a : Nat) -> (r : Address) -> {auto p: LTE a b} -> Eff ()
       [ETH v b]
       [ETH v (b-a)]
send a r = call $ Send a r

