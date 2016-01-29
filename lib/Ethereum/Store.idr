module Ethereum.Store

import Data.Vect
import Data.HVect
import Effects
import Ethereum.Types

-- %default total

---- TYPES ----
VarName : Type
VarName = String

data Field    = EInt VarName 
data MapField = EMIntInt VarName | EMAddressInt VarName | EMIntAddress VarName

Show Field where
  show (EInt n)     = "EINT_" ++ n

Show MapField where
  show (EMIntInt n) = "EMII_" ++ n
  show (EMAddressInt n) = "EMAI_" ++ n
  show (EMIntAddress n) = "EMIA_" ++ n

namespace Field
  name : Field -> VarName
  name (EInt n) = n

namespace MapField
  name : MapField -> VarName
  name (EMIntInt n) = n
  name (EMAddressInt n) = n
  name (EMIntAddress n) = n

InterpField : Field -> Type
InterpField (EInt _) = Int

InterpMapKey : MapField -> Type
InterpMapKey (EMIntInt _)     = Int
InterpMapKey (EMAddressInt _) = Address
InterpMapKey (EMIntAddress _) = Int

InterpMapVal : MapField -> Type
InterpMapVal (EMIntInt _)    = Int
InterpMapVal (EMAddressInt _) = Int
InterpMapVal (EMIntAddress _) = Address

---- EFFECT ----
data Store : Effect where
  Read     : (f : Field) -> sig Store (InterpField f) ()
  ReadMap  : (f : MapField) -> (InterpMapKey f) -> sig Store (InterpMapVal f) ()
  Write    : (f : Field) -> (InterpField f) -> sig Store () ()
  WriteMap : (f : MapField) -> (InterpMapKey f) -> (InterpMapVal f) -> sig Store () ()

STORE : EFFECT
STORE = MkEff () Store

namespace Field
  read : (f : Field) -> Eff (InterpField f) [STORE]
  read f = call $ Read f

  write : (f : Field) -> (InterpField f) -> Eff () [STORE]
  write f x = call (Write f x)

  update : (f : Field) -> (InterpField f -> InterpField f) -> Eff () [STORE]
  update f fun = write f (fun !(read f))

namespace MapField
  read : (f : MapField) -> (InterpMapKey f) -> Eff (InterpMapVal f) [STORE]
  read f k = call $ ReadMap f k

  write : (f : MapField) -> (InterpMapKey f) -> (InterpMapVal f) -> Eff () [STORE]
  write f k x = call (WriteMap f k x)

  update : (f : MapField) -> (InterpMapKey f) -> (InterpMapVal f -> InterpMapVal f) -> Eff () [STORE]
  update f k fun = write f k (fun !(read f k))

