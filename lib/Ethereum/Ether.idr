module Ethereum.Ether

import Effects
import Ethereum.Types
import Ethereum.GeneralStore

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


syntax [res] "{" "VALUE" "=" [v] ";" "BALANCE" "=" [b] ";" "SEND" "=" [t] ";" "SAVE" "=" [s] "}" = TransEff.Eff res [ETH v b 0 0] [ETH v b t s]

syntax [res] "{" "VALUE" "=" [v] ";" "BALANCE" "=" [b] ";" "SEND" "=" [t] ";" "SAVE" "=" [s] ";" [effs] "}" = TransEff.Eff res (ETH v b 0 0 :: effs) (ETH v b t s :: effs)

syntax [restype] "{" {res} "=>" "VALUE" "=" [v] ";" "BALANCE" "=" [b] ";" "SEND" "=" [t] ";" "SAVE" "=" [s] "}" = DepEff.Eff restype [ETH v b 0 0] (\res => [ETH v b t s])

syntax [restype] "{" {res} "=>" "VALUE" "=" [v] ";" "BALANCE" "=" [b] ";" "SEND" "=" [t] ";" "SAVE" "=" [s] ";" [effs] "}" = DepEff.Eff restype (ETH v b 0 0 :: effs) (\res => ETH v b t s :: effs)


addPlayer : Int -> {auto p: LTE 10 value} -> Bool { success => 
                                                    VALUE   = value
                                                  ; BALANCE = balance
                                                  ; SEND    = if success then value-10 else value
                                                  ; SAVE    = if success then 10 else 0
                                                  ; [STORE] }
addPlayer = believe_me

--         , SEND    : (\success => if success then value - 10 else value) , SAVE : (\success => if success then 10 else 0 ) }
{-
syntax [restype] "{" {res} "=>" "VALUE" "=" [v] ";" "BALANCE" "=" [b] ";" "SEND" "=" [t] ";" "SAVE" "=" [s] "}" = DepEff.Eff restype [ETH v b 0 0] (\ress => [ETH v b (t ress) (s ress)])


addPlayer : Int -> Bool { success =>
                          VALUE   = value
                        ; BALANCE = balance
                        ; SEND    = if success then (value-10) else value
                        ; SAVE    = 0 }
addPlayer = believe_me
-}