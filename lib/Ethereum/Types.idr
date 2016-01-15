module Ethereum.Types

import Data.Vect
import Data.HVect
import Data.Vect.Quantifiers

%default total
%access public

-- Missing head, tail function for HVect
head : HVect (t::ts) -> t
head (x::_) = x

tail : HVect (t::ts) -> HVect ts
tail (_::xs) = xs

VarName : Type
VarName = String

Address : Type
Address = Int

-- TODO: Lists, maps
-- parameter: index
data Field    = EInt VarName -- | EString String | EAddress Nat -- | EArray Nat Nat Field
data MapField = EMIntInt VarName 

instance Show Field where
  show (EInt n)     = "EINT_" ++ show n
--  show (EString n)  = "ESTRING_" ++ show n
--  show (EAddress n) = "EADDRESS_" ++ show n
--  show (EArray n l t) = "EARRAY_" ++ show n

namespace Field
  name : Field -> VarName
  name (EInt n) = n

namespace MapField
  name : MapField -> VarName
  name (EMIntInt n) = n

-- index : Field -> Nat
-- index (EInt n)     = n
-- index (EString n)  = n
-- index (EAddress n) = n
-- 
-- size : Field -> Nat
-- size (EInt _)     = 1
-- size (EString _)  = 1
-- size (EAddress _) = 1
--size (EArray _ l t) = l*size t

--Schema definition
Schema : Nat -> Type
Schema k = Vect k Field

InterpField : Field -> Type
InterpField (EInt _) = Int
--InterpField (EString _) = String
--InterpField (EAddress _) = Integer
--InterpField (EArray _ l t) = Vect l (InterpField t)

InterpMapKey : MapField -> Type
InterpMapKey (EMIntInt _) = Int

InterpMapVal : MapField -> Type
InterpMapVal (EMIntInt _) = Int

-- Interpretation function: takes Schema and creates type
Interp : Schema k -> Type
Interp schema = HVect (map InterpField schema)

