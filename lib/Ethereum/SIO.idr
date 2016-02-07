module Ethereum.SIO

import public Effects
import public Ethereum.Types
import public Ethereum.Store
import public Ethereum.Ether
import public Ethereum.Environment

%default total
%access public

-- Supported foreign types
data SeTypes : Type -> Type where
  -- Primitive types
  SeInt_io    : SeTypes Int
  SeNat_io    : SeTypes Nat
  SeBool_io   : SeTypes Bool
  SeChar_io   : SeTypes Char
  SeString_io : SeTypes String

  -- Other types
  SeUnit_io  : SeTypes ()
  SeMaybe_io : SeTypes (Maybe a)
  SeFun_io   : SeTypes a -> SeTypes b -> SeTypes (a -> b)

  -- Arbitrary Idris objects, opaque to Serpent
  SeAny_io   : SeTypes (FFI_C.Raw a)

FFI_Se : FFI
FFI_Se = MkFFI SeTypes String String

SIO : Type -> Type
SIO = IO' FFI_Se

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

%extern prim__read : (f : Field) -> (InterpField f)
%extern prim__write : (f : Field) -> (InterpField f) ->  ()
%extern prim__readMap : (f : MapField) -> InterpMapKey f -> (InterpMapVal f)
%extern prim__writeMap : (f : MapField) -> InterpMapKey f -> InterpMapVal f -> ()


---------------------
-- Effect Handlers --
---------------------

Handler EnvRules m where
  handle state@(MkEnv c _ _) Self         k = k c state
  handle state@(MkEnv _ s _) Sender       k = k s state
  handle state@(MkEnv _ _ o) Origin       k = k o state
  handle state             RemainingGas k = k prim__remainingGas state
  handle state             TimeStamp    k = k prim__timestamp state
  handle state             Coinbase     k = k prim__coinbase state

Handler EtherRules m where
  handle state@(MkEth v _ _ _) Value           k = k v state
  handle state@(MkEth _ b _ _) ContractBalance k = k b state
  handle state               (Balance a)     k = k (prim__balance a) state
  handle (MkEth v b t s)       (Save a)        k = k () (MkEth v b t (s+a))
  handle (MkEth v b t s)       (Send a r)      k = k (prim__send r a) (MkEth v b (t+a) s)

Handler Store m where
  handle s (Read field)             k = k (prim__read field) s
  handle s (Write field val)        k = k (prim__write field val) s
  handle s (ReadMap field key)      k = k (prim__readMap field key) s
  handle s (WriteMap field key val) k = k (prim__writeMap field key val) s

