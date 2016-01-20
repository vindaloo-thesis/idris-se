module Ethereum.Types

import Data.Vect
import Data.HVect
import Effect.Default

%default total
%access public

VarName : Type
VarName = String

Address : Type
Address = Int

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

