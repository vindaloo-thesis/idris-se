module Ethereum.Ether

import Effects
import Data.Fin
import Data.So
import Effect.StdIO
import Effect.Exception
import Control.IOExcept
import Ethereum.SIO
import Ethereum.Types

------------ TYPES -----------------
data Commit a = Comm a

-------------- EFFECT --------------
data CState = NotRunning | Running Nat Nat Nat
Init : Nat -> CState
Init v = Running v 0 0

Finished : {v : Nat} -> Nat -> Nat -> CState
Finished {v} t s = Running v t s

data Ether : CState -> Type where
  MkS : (value: Nat) -> (trans: Nat) -> (saved: Nat) -> Ether (Running value trans saved)

instance Default CState where
  default = Init 1

instance Default (Ether (Running v 0 0)) where
  default {v} = MkS v 0 0 

--TODO: Can we remove Finish here and just use Running?
data EtherRules : Effect where
  ContractAddress : sig EtherRules Address
                    (Ether (Running v t s))
  Value   : sig EtherRules Nat
            (Ether (Running v t s))
  Balance : Address -> sig EtherRules Nat
            (Ether (Running v t s))
  Sender   : sig EtherRules Address
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

Contract : (x : Type) -> (ce : x -> List EFFECT) -> Type
Contract x ce = {m : Type -> Type} -> {v : Nat} -> EffM m x [ETH (Init v)] ce

instance Handler EtherRules IO where
  handle (MkS v t s) Value    k = k v (MkS v t s)

  handle (MkS v t s) ContractAddress k = k 0x00000000000000000000000000000000deadbeef (MkS v t s)

  handle (MkS v t s) (Balance a) k = k 100 (MkS v t s) -- TODO: Change this. Balance should be *read*.

  handle (MkS v t s) (Save a) k = do putStrLn $ "- Saved " ++ show a
                                     k () (MkS v t (s+a))

  handle (MkS v t s) (Send a r) k = do putStrLn $ "- Sent  " ++ show a ++ " to " ++ show r
                                       k () (MkS v (t+a) s)

  handle (MkS v t s) Sender k   = k 0x00cf7667b8dd4ece1728ef7809bc844a1356aadf (MkS v t s)


instance Handler EtherRules SIO where
  handle (MkS v t s) Value    k = k v (MkS v t s)

  handle (MkS v t s) ContractAddress k = k 0x00000000000000000000000000000000deadbeef (MkS v t s)

  handle (MkS v t s) (Balance a) k = k 100 (MkS v t s) -- TODO: Change this. Balance should be *read*.

  handle (MkS v t s) (Save a) k = do --putStrLn $ "- Saved " ++ show a
                                     k () (MkS v t (s+a))

  handle (MkS v t s) (Send a r) k = do --putStrLn $ "- Sent  " ++ show a ++ " to " ++ show r
                                       k () (MkS v (t+a) s)

  handle (MkS v t s) Sender k   = do
    s <- sender
    k s (MkS v t s)
    --k 0x00cf7667b8dd4ece1728ef7809bc844a1356aadf (MkS v t s)
ETH_IN : Nat -> EFFECT
ETH_IN v = ETH (Init v)

ETH_OUT : Nat -> Nat -> Nat -> EFFECT
ETH_OUT v t s = ETH (Finished {v} t s)

contractAddress : Eff Address
       [ETH (Running v t s)]
contractAddress = call $ ContractAddress

sender : Eff Address
       [ETH (Running v t s)]
sender = call $ Sender

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

