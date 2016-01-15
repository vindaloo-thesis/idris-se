module Ethereum.SIO

--import Python.Objects
import Ethereum.Types

%default total
%access public

unRaw : FFI_C.Raw a -> a
unRaw (MkRaw x) = x

||| Supported Python foreign types.
data SeTypes : Type -> Type where

  -- Primitive types
  SeInt_io     : SeTypes Int
--  PyNat_io     : PyTypes Nat
--  PyInteger_io : PyTypes Integer
--  PyDouble_io  : PyTypes Double
  SeBool_io    : SeTypes Bool
  SeChar_io    : SeTypes Char
  SeString_io  : SeTypes String
--
--  -- Other types
  SeUnit_io    : SeTypes ()
  SeFun_io   : SeTypes a -> SeTypes b -> SeTypes (a -> b)
--  PyUnit_io  : PyTypes ()
--  PyPair_io  : PyTypes a -> PyTypes b -> PyTypes (a, b)
--  PyList_io  : PyTypes a -> PyTypes (List a)
--  PyFun_io   : PyTypes a -> PyTypes b -> PyTypes (a -> b)
--  PyMaybe_io : PyTypes a -> PyTypes (Maybe a)
--
--  ||| Python objects, opaque to Idris.
--  PyPtr_io       : PyTypes Ptr
--
--  ||| Arbitrary Idris objects, opaque to Python.
  SeAny_io : SeTypes (FFI_C.Raw a)
--
--  ||| Python objects with a signature known to Idris.
--  PyObj_io : PyTypes (Obj sig)

FFI_Se : FFI
FFI_Se = MkFFI SeTypes String String

||| Serpent IO.
SIO : Type -> Type
SIO = IO' FFI_Se

balance : Address -> SIO Int
balance a = foreign FFI_Se "getBalance" (Address -> SIO Int) a

sender : SIO Address
sender = foreign FFI_Se "msg.sender" (SIO Int)

send : Address -> Nat -> SIO ()
send a n = foreign FFI_Se "send" (Address -> Int -> SIO ()) a (toIntNat n)

contractAddress : SIO Address
contractAddress = foreign FFI_Se "self" (SIO Address)

remainingGas : SIO Nat
remainingGas = toNat <$> foreign FFI_Se "msg.gas" (SIO Int)

se_read : (f : Field) -> SIO (InterpField f)
se_read f = unRaw <$> foreign FFI_Se "readVal" (VarName -> SIO (Raw (InterpField f))) (name f)


se_readMap : (f : MapField) -> InterpMapKey f -> SIO (InterpMapVal f)
se_readMap f k = unRaw <$> foreign FFI_Se "readMap" (VarName -> ( Raw (InterpMapKey f)) -> SIO (Raw (InterpMapVal f))) (name f) (MkRaw k)

se_writeMap : (f : MapField) -> InterpMapKey f -> InterpMapVal f -> SIO ()
se_writeMap (EMIntInt n) k val = foreign FFI_Se "writeMap" (VarName -> Int -> Int -> SIO ()) n k val

readInt : (f : Field) -> SIO (Int)
readInt f = foreign FFI_Se "readVal" (VarName -> SIO (Int)) (name f)

se_write : (f : Field) -> (InterpField f) -> SIO ()
se_write (EInt n) val = foreign FFI_Se "writeVal" (VarName -> Int -> SIO ()) n val

