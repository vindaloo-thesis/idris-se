module Ethereum.Ether

import Effects
import Ethereum.Types

-------------- EFFECT --------------
data Ether : Nat -> Nat -> Nat -> Nat -> Type where
  MkEth : (value : Nat)   -> -- Incoming ether (call)
          (balance : Nat) -> -- Ingoing contract balance (call)
          (trans : Nat)   -> -- Ether sent in outgoing transactions
          (kept : Nat)   -> -- Part of incoming ether explicitly kept to contract
          Ether value balance trans kept

data EtherRules : Effect where
  -- Incoming ether (call)
  Value   : sig EtherRules Nat 
            (Ether v b t k)
  -- Ingoing contract balance (call)
  ContractBalance : sig EtherRules Nat
            (Ether v b t k)
  -- Balance of a given address
  Balance : Address -> sig EtherRules Nat
            (Ether v b t k)
  -- Explicitly keep amount of incoming ether
  Keep    : (a : Nat) -> 
            sig EtherRules ()
            (Ether v b t k)
            (Ether v b t (k+a))
  -- Send ether to address
  Send    : (a : Nat) ->
            (r : Address) ->
            sig EtherRules ()
            (Ether v b t k)
            (Ether v b (t+a) k)

ETH : Nat -> Nat -> Nat -> Nat -> EFFECT
ETH v b t k = MkEff (Ether v b t k) EtherRules

value : Eff Nat
       [ETH v b t k]
value = call Value

balance : Address -> Eff Nat
       [ETH v b t k]
balance a = call $ Balance a

contractBalance : Eff Nat
       [ETH v b t k]
contractBalance = call ContractBalance

keep : (a : Nat) -> Eff ()
       [ETH v b t k]
       [ETH v b t (k+a)]
keep a = call $ Keep a

send : (a : Nat) -> (r : Address) -> Eff ()
       [ETH v b t k]
       [ETH v b (t+a) k]
send a r = call $ Send a r
