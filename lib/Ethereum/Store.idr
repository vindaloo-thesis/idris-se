module Ethereum.Store

import Data.Vect
import Data.HVect
import Effects
import Ethereum.Types

%default total

---- TYPES ----
VarName : Type
VarName = Nat


namespace Field
  data Field : Type -> Type where
    MkField  : VarName -> Field a

  name : Field a -> VarName
  name  (MkField n) = n

namespace MapField
  data Field : Type -> Type -> Type where
    MkField : VarName -> Field a b

  name : Field a b -> VarName
  name (MkField n) = n

---- EFFECT ----
data Store : Effect where
  Read     : Field a -> sig Store a ()
  ReadMap  : Field a b -> a -> sig Store b ()
  Write    : Field a -> a -> sig Store () ()
  WriteMap : Field a b -> a -> b -> sig Store () ()

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
  read : Field a b -> a -> Eff b [STORE]
  read f k = call $ ReadMap f k

  write : Field a b -> a -> b -> Eff () [STORE]
  write f k x = call (WriteMap f k x)

  --TODO: This doesn't work when we hack away EVAL/APPLY. But neither does the alternative.
  update : Field a b -> a -> (b -> b) -> Eff () [STORE]
  update f k fun = write f k (fun !(read f k))

