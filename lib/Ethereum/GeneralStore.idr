module GeneralStore

import Effects
--import Effect.StdIO
import Data.Vect
import Data.HVect
import Types


data Store : Effect where
  Read  : (f : Field) -> sig Store (InterpField f) ()
  Write : (f : Field) -> (InterpField f) -> sig Store () ()

STORE : EFFECT
STORE = MkEff () Store

read : (f : Field) -> Eff (InterpField f) [STORE]
read f = call $ GeneralStore.Read f

write : (f : Field) -> (InterpField f) -> Eff () [STORE]
write f x = call (Write f x)

deserialize : (f : Field) -> String -> InterpField f
deserialize (EInt _)  = cast . prim__fromStrInt 
deserialize (EString _) = id
deserialize (EAddress _) = cast . prim__fromStrInt 

serialize : (f : Field) -> InterpField f -> String
serialize (EInt _) x = show x
serialize (EString _) x = x
serialize (EAddress _) x = show x
{-
serialize (EArray _ l t) xs = "[" ++ serialize' "" xs ++ "]" where
    serialize' : String -> Vect l' (InterpField t) -> String
    serialize' acc []        = acc
    serialize' acc [x]       = acc ++ serialize t x
    serialize' acc (x :: xs) = serialize' (acc ++ serialize t x ++ ",") xs
-}

-- TODO: Error handler for when files don't exist etc
instance Handler Store IO where
  handle s (Read field)     k =
    do
      Right val <- readFile "(show field)"
      putStrLn $ "- Read " ++ show field ++ ": " ++ trim "val"
      k (deserialize field (trim val)) s
  handle s (Write field val) k =
    do
      putStrLn $ "- Write " ++ show field ++ " = " ++ serialize field val 
      writeFile (show field) (serialize field val)
      k () s







