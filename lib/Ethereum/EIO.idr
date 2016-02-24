module Ethereum.EIO

import public Effects
import public Ethereum.Types
import public Ethereum.Store
import public Ethereum.Ether
import public Ethereum.Environment

%default total
%access public

-- Supported foreign types
data EthTypes : Type -> Type where
  -- Primitive types
  EthInt_io    : EthTypes Int
  EthNat_io    : EthTypes Nat
  EthBool_io   : EthTypes Bool
  EthChar_io   : EthTypes Char
  EthString_io : EthTypes String

  -- Other types
  EthUnit_io  : EthTypes ()
  EthMaybe_io : EthTypes (Maybe a)

FFI_Eth : FFI
FFI_Eth = MkFFI EthTypes String String

EIO : Type -> Type
EIO = IO' FFI_Eth

%extern prim__value        : Nat
%extern prim__selfbalance  : Nat
%extern prim__balance      : Address -> Nat
%extern prim__send         : Address -> Nat -> ()

%extern prim__self         : Address
%extern prim__sender       : Address
%extern prim__origin       : Address
%extern prim__remainingGas : Nat
%extern prim__timestamp    : Nat
%extern prim__coinbase     : Address

%extern prim__read         : Field a -> a
%extern prim__write        : Field a -> a ->  ()
%extern prim__readMap      : Field a b -> a -> b
%extern prim__writeMap     : Field a b -> a -> b -> ()


---------------------
-- Effect Handlers --
---------------------

Handler EnvRules m where
  handle state@(MkEnv _ _) Self         k = k prim__self state
  handle state@(MkEnv s _) Sender       k = k s state
  handle state@(MkEnv _ o) Origin       k = k o state
  handle state               RemainingGas k = k prim__remainingGas state
  handle state               TimeStamp    k = k prim__timestamp state
  handle state               Coinbase     k = k prim__coinbase state

Handler EtherRules m where
  handle state@(MkEth v _ _ _) Value           k = k v state
  handle state@(MkEth _ b _ _) ContractBalance k = k b state
  handle state                 (Balance a)     k = k (prim__balance a) state
  handle (MkEth v b t kept)       (Keep a)     k = k () (MkEth v b t (kept+a))
  handle (MkEth v b t kept)       (Send a r)   k = k (prim__send r a) (MkEth v b (t+a) kept)

Handler Store m where
  handle s (Read field)             k = k (prim__read field) s
  handle s (Write field val)        k = k (prim__write field val) s
  handle s (ReadMap field key)      k = k (prim__readMap field key) s
  handle s (WriteMap field key val) k = k (prim__writeMap field key val) s

