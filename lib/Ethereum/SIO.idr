module Ethereum.SIO

--import Python.Objects
import Types

%default total
%access public

--unRaw : FFI_C.Raw a -> a
--unRaw (MkRaw x) = x

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
--  PyAny_io : PyTypes (FFI_C.Raw a)
--
--  ||| Python objects with a signature known to Idris.
--  PyObj_io : PyTypes (Obj sig)

FFI_Se : FFI
FFI_Se = MkFFI SeTypes String String

||| Serpent IO.
SIO : Type -> Type
SIO = IO' FFI_Se

se_read : (f : Field) -> SIO (InterpField f)
se_read f = foreign FFI_Se "readVal" (VarName -> SIO (InterpField f)) (name f)

se_write : (f : Field) -> (InterpField f) -> SIO ()
se_write f val = foreign FFI_Se "writeVal" (VarName -> InterpField f -> SIO ()) (name f) val

