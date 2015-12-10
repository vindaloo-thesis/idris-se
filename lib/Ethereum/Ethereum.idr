module Effects.Ethereum

import Effects
import Data.Fin
import Data.So
import Effect.StdIO
import Effect.Exception
import Control.IOExcept

------------ TYPES -----------------
data Commit a = Comm a

Address : Type
Address = Integer

-------------- EFFECT --------------
data CState = NotRunning | Running Nat Nat Nat
Init : Nat -> CState
Init v = Running v 0 0

Finished : {v : Nat} -> Nat -> Nat -> CState
Finished {v} t s = Running v t s

data Ethereum : CState -> Type where
  MkS : (value: Nat) -> (trans: Nat) -> (saved: Nat) -> Ethereum (Running value trans saved)

instance Default CState where
  default = Init 1

instance Default (Ethereum (Running v 0 0)) where
  default {v} = MkS v 0 0 

--TODO: Can we remove Finish here and just use Running?
data EthereumRules : Effect where
  ContractAddress : sig EthereumRules Address
                    (Ethereum (Running v t s))
  Value   : sig EthereumRules Nat
            (Ethereum (Running v t s))
  Balance : Address -> sig EthereumRules Nat
            (Ethereum (Running v t s))
  Sender   : sig EthereumRules Address
            (Ethereum (Running v t s))
  Save    : (a : Nat) -> 
            sig EthereumRules ()
            (Ethereum (Running v t s))
            (Ethereum (Running v t (s+a)))
  Send    : (a : Nat) ->
            (r : Address) ->
            sig EthereumRules ()
            (Ethereum (Running v t s))
            (Ethereum (Running v (t+a) s))

ETHEREUM : CState -> EFFECT
ETHEREUM h = MkEff (Ethereum h) EthereumRules

Contract : (x : Type) -> (ce : x -> List EFFECT) -> Type
Contract x ce = {m : Type -> Type} -> {v : Nat} -> EffM m x [ETHEREUM (Init v)] ce

instance Handler EthereumRules IO where
  handle (MkS v t s) Value    k = k v (MkS v t s)

  handle (MkS v t s) ContractAddress k = k 0x00000000000000000000000000000000deadbeef (MkS v t s)

  handle (MkS v t s) (Balance a) k = k 100 (MkS v t s) -- TODO: Change this. Balance should be *read*.

  handle (MkS v t s) (Save a) k = do putStrLn $ "- Saved " ++ show a
                                     k () (MkS v t (s+a))

  handle (MkS v t s) (Send a r) k = do putStrLn $ "- Sent  " ++ show a ++ " to " ++ show r
                                       k () (MkS v (t+a) s)

  handle (MkS v t s) Sender k   = k 0x00cf7667b8dd4ece1728ef7809bc844a1356aadf (MkS v t s)


ETH_IN : Nat -> EFFECT
ETH_IN v = ETHEREUM (Init v)

ETH_OUT : Nat -> Nat -> Nat -> EFFECT
ETH_OUT v t s = ETHEREUM (Finished {v} t s)

contractAddress : Eff Address
       [ETHEREUM (Running v t s)]
contractAddress = call $ ContractAddress

sender : Eff Address
       [ETHEREUM (Running v t s)]
sender = call $ Sender

value : Eff Nat
       [ETHEREUM (Running v t s)]
value = call $ Value

balance : Address -> Eff Nat
       [ETHEREUM (Running v t s)]
balance a = call $ (Balance a)

save : (a : Nat) -> Eff ()
       [ETHEREUM (Running v t s)]
       [ETHEREUM (Running v t (s+a))]
save a = call $ Save a

send : (a : Nat) -> (r : Address) -> Eff ()
       [ETHEREUM (Running v t s)]
       [ETHEREUM (Running v (t+a) s)]
send a r = call $ Send a r

