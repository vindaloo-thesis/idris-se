module Ethereum.Store

import Data.Vect
import Data.HVect
import Effects
import Ethereum.Types

%default total

---- TYPES ----
VarName : Type
VarName = Nat

interface Serialize (a : Type) where
  serialize   : a -> String
  deserialize : String -> a
  defVal : a

record Field a where
  constructor MkField
  name : VarName

record MapField a b where
  constructor MkMapField
  name : VarName

Serialize Int where
  serialize = show
  deserialize = prim__fromStrInt 
  defVal = 0

Serialize Address where
  serialize = show
  deserialize = prim__fromStrBigInt 
  defVal = 0

Serialize String where
  serialize = id
  deserialize = id
  defVal = ""

Show (Field a) where
  show f = "EF_" ++ show (name f)

Show (MapField a b) where
  show f = "EMF_" ++ show (name f)

---- EFFECT ----
data Store : Effect where
  Read     : Field a -> sig Store a ()
  ReadMap  : MapField a b -> a -> sig Store b ()
  Write    : Field a -> a -> sig Store () ()
  WriteMap : MapField a b -> a -> b -> sig Store () ()

STORE : EFFECT
STORE = MkEff () Store

namespace Field
  read : Field a -> Eff a [STORE]
  read f = call $ Read f

  write : Field a -> a -> Eff () [STORE]
  write f x = call (Write f x)

  --TODO: This doesn't work when we hack away EVAL/APPLY. But neither does the alternative.
  update : Field a -> (a -> a) -> Eff () [STORE]
  update f fun = write f (fun !(read f))

namespace MapField
  read : MapField a b -> a -> Eff b [STORE]
  read f k = call $ ReadMap f k

  write : MapField a b -> a -> b -> Eff () [STORE]
  write f k x = call (WriteMap f k x)

  --TODO: This doesn't work when we hack away EVAL/APPLY. But neither does the alternative.
  update : MapField a b -> a -> (b -> b) -> Eff () [STORE]
  update f k fun = write f k (fun !(read f k))

