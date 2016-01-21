module Ethereum.SIO

import Effects
import Ethereum.Types
import Ethereum.Store
import Ethereum.Ether
import Ethereum.Environment

%default total
%access public

%extern prim__value : Nat
%extern prim__balance : Nat

unRaw : FFI_C.Raw a -> a
unRaw (MkRaw x) = x

-- Supported Python foreign types.
data SeTypes : Type -> Type where
  -- Primitive types
  SeInt_io    : SeTypes Int
  SeBool_io   : SeTypes Bool
  SeChar_io   : SeTypes Char
  SeString_io : SeTypes String

  -- Other types
  SeUnit_io  : SeTypes ()
  SeFun_io   : SeTypes a -> SeTypes b -> SeTypes (a -> b)

  -- Arbitrary Idris objects, opaque to Serpent.
  SeAny_io   : SeTypes (FFI_C.Raw a)

FFI_Se : FFI
FFI_Se = MkFFI SeTypes String String

SIO : Type -> Type
SIO = IO' FFI_Se

-- Ether
value : SIO Nat
value = toNat <$> foreign FFI_Se "msg.value" (SIO Int)

balance : Address -> SIO Int
balance a = foreign FFI_Se "getBalance" (Address -> SIO Int) a

send : Address -> Nat -> SIO ()
send a n = foreign FFI_Se "send" (Address -> Int -> SIO ()) a (toIntNat n)

sender : SIO Address
sender = foreign FFI_Se "msg.sender" (SIO Int)

-- Environment
contractAddress : SIO Address
contractAddress = foreign FFI_Se "self" (SIO Address)

coinbase : SIO Address
coinbase = foreign FFI_Se "block.coinbase" (SIO Address)

remainingGas : SIO Nat
remainingGas = toNat <$> foreign FFI_Se "msg.gas" (SIO Int)

contractBalance : SIO Nat
contractBalance = toNat <$> foreign FFI_Se "self.balance" (SIO Int)

timestamp : SIO Nat
timestamp = toNat <$> foreign FFI_Se "block.timestamp" (SIO Int)

-- Store
se_read : (f : Field) -> SIO (InterpField f)
se_read f = unRaw <$> foreign FFI_Se "readVal" (VarName -> SIO (Raw (InterpField f))) (name f)

se_write : (f : Field) -> (InterpField f) -> SIO ()
se_write (EInt n) val = foreign FFI_Se "writeVal" (VarName -> Int -> SIO ()) n val

se_readMap : (f : MapField) -> InterpMapKey f -> SIO (InterpMapVal f)
se_readMap f k = unRaw <$> foreign FFI_Se "readMap" (VarName -> ( Raw (InterpMapKey f)) -> SIO (Raw (InterpMapVal f))) (name f) (MkRaw k)

se_writeMap : (f : MapField) -> InterpMapKey f -> InterpMapVal f -> SIO ()
se_writeMap (EMIntInt n) k val = foreign FFI_Se "writeMap" (VarName -> Int -> Int -> SIO ()) n k val

---------------------
-- Effect Handlers --
---------------------

instance Handler EnvRules SIO where
  handle state@(MkE c _ _) ContractAddress k = k c state
  handle state@(MkE _ s _) Sender          k = k s state
  handle state@(MkE _ _ o) Origin          k = k o state
  handle state RemainingGas                k = do
    gas <- remainingGas
    k gas state
  handle state TimeStamp                   k = do
    time <- timestamp
    k time state
  handle state@(MkE _ _ o) Coinbase        k = do
    cb <- coinbase
    k cb state

instance Handler EtherRules SIO where
  handle state@(MkS v _ _ _) Value           k = k v state
  handle state@(MkS _ b _ _) ContractBalance k = k b state
  handle state (Balance a)                   k = do
    bal <- balance a
    k (toNat bal) state
  handle (MkS v b t s) (Save a)    k = k () (MkS v b t (s+a))
  handle (MkS v b t s) (Send a r)  k = do
    send r a
    k () (MkS v b (t+a) s)

instance Handler Store SIO where
  handle s (Read field)             k = do
      val <- se_read field
      k val s
  handle s (Write field val)        k = do
      se_write field val
      k () s
  handle s (ReadMap field key)      k = do
      val <- se_readMap field key
      k val s
  handle s (WriteMap field key val) k = do
      se_writeMap field key val
      k () s

