module Ethereum.Store

import Data.Vect
import Data.HVect
import Effects
import Ethereum.Types

%default total

---- TYPES ----
VarName : Type
VarName = String

data Field    = EInt VarName 
data MapField = EMIntInt VarName 

instance Show Field where
  show (EInt n)     = "EINT_" ++ n

instance Show MapField where
  show (EMIntInt n)     = "EMINT_" ++ n

namespace Field
  name : Field -> VarName
  name (EInt n) = n

namespace MapField
  name : MapField -> VarName
  name (EMIntInt n) = n

--Schema definition
Schema : Nat -> Type
Schema k = Vect k Field

InterpField : Field -> Type
InterpField (EInt _) = Int

InterpMapKey : MapField -> Type
InterpMapKey (EMIntInt _) = Int

InterpMapVal : MapField -> Type
InterpMapVal (EMIntInt _) = Int

-- Interpretation function: takes Schema and creates type
Interp : Schema k -> Type
Interp schema = HVect (map InterpField schema)

---- EFFECT ----
data Store : Effect where
  Read  : (f : Field) -> sig Store (InterpField f) ()
  ReadMap  : (f : MapField) -> (InterpMapKey f) -> sig Store (InterpMapVal f) ()
  Write : (f : Field) -> (InterpField f) -> sig Store () ()
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

  defVal : (f: Field) -> InterpField f
  defVal (EInt _) = 0

  serialize : (f : Field) -> InterpField f -> String
  serialize (EInt _) = show

  deserialize : (f : Field) -> String -> InterpField f
  deserialize (EInt _)  = prim__fromStrInt 

namespace MapField
  read : (f : MapField) -> (InterpMapKey f) -> Eff (InterpMapVal f) [STORE]
  read f k = call $ ReadMap f k

  write : (f : MapField) -> (InterpMapKey f) -> (InterpMapVal f) -> Eff () [STORE]
  write f k x = call (WriteMap f k x)

  defVal : (f: MapField) -> InterpMapVal f
  defVal (EMIntInt _) = 0

  serialize : (f : MapField) -> InterpMapVal f -> String
  serialize (EMIntInt _) = show

  deserialize : (f : MapField) -> String -> InterpMapVal f
  deserialize (EMIntInt _)  = prim__fromStrInt 

