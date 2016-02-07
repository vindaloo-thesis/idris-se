module Ethereum.Ether

import Effects
import Ethereum.Types

-------------- EFFECT --------------
data Ether : Nat -> Nat -> Nat -> Nat -> Type where
  MkEth : (value: Nat)   -> -- Incoming ether (call)
          (balance: Nat) -> -- Ingoing contract balance (call)
          (trans: Nat)   -> -- Ether sent in outgoing transactions
          (saved: Nat)   -> -- Part of incoming ether explicitly saved to contract
          Ether value balance trans saved

data EtherRules : Effect where
  -- Incoming ether (call)
  Value   : sig EtherRules Nat 
            (Ether v b t s)
  -- Ingoing contract balance (call)
  ContractBalance : sig EtherRules Nat
            (Ether v b t s)
  -- Balance of a given address
  Balance : Address -> sig EtherRules Nat
            (Ether v b t s)
  -- Explicitly save amount of incoming ether
  Save    : (a : Nat) -> 
            sig EtherRules ()
            (Ether v b t s)
            (Ether v b t (s+a))
  -- Send ether to address
  Send    : (a : Nat) ->
            (r : Address) ->
            sig EtherRules ()
            (Ether v b t s)
            (Ether v b (t+a) s)

ETH : Nat -> Nat -> Nat -> Nat -> EFFECT
ETH v b t s = MkEff (Ether v b t s) EtherRules

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
