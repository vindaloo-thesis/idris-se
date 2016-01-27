module Ethereum.IO

import Effects
import Ethereum.Types
import Ethereum.Store
import Ethereum.Ether
import Ethereum.Environment

Handler EnvRules IO where
  handle state@(MkE c _ _) Self     k = k c state
  handle state@(MkE _ s _) Sender   k = k s state
  handle state@(MkE _ _ o) Origin   k = k o state
  handle state RemainingGas         k = k 100 state
  handle state TimeStamp            k = k 1453299096 state
  handle state@(MkE _ _ o) Coinbase k = k o state

Handler EtherRules IO where
  handle state@(MkS v _ _ _) Value           k = k v   state
  handle state@(MkS _ b _ _) ContractBalance k = k b   state
  handle state               (Balance a)     k = k 100 state
  handle (MkS v b t s)       (Save a)        k = do putStrLn $ "- Saved " ++ show a
                                                    k () (MkS v b t (s+a))
  handle (MkS v b t s)       (Send a r) k = do putStrLn $ "- Sent  " ++ show a ++ " to " ++ show r
                                               k () (MkS v b (t+a) s)

namespace Field
  private
  serialize : (f : Field) -> InterpField f -> String
  serialize (EInt _) = show

  private
  deserialize : (f : Field) -> String -> InterpField f
  deserialize (EInt _)  = prim__fromStrInt 

  private 
  defVal : (f: Field) -> InterpField f
  defVal (EInt _) = 0

namespace MapField
  private
  serialize : (f : MapField) -> InterpMapVal f -> String
  serialize (EMIntInt _) = show
  serialize (EMAddressInt _) = show

  private
  deserialize : (f : MapField) -> String -> InterpMapVal f
  deserialize (EMIntInt _)  = prim__fromStrInt 
  deserialize (EMAddressInt _)  = prim__fromStrInt 

  private
  defVal : (f: MapField) -> InterpMapVal f
  defVal (EMIntInt _) = 0
  defVal (EMAddressInt _) = 0

Handler Store IO where
  handle s (Read field)     k =
    do
      f <- readFile $ show field
      case f of
           Right val => do
             putStrLn $ "- Read " ++ show field ++ ": " ++ trim val
             k (deserialize field val) s
           Left _ => do
             putStrLn $ "Error reading file for " ++ show field 
             k (defVal field) s
                  
  handle s (Write field val) k =
    do
      putStrLn $ "- Write " ++ show field ++ " = " ++ serialize field val 
      writeFile (show field) (serialize field val)
      k () s

  handle s (ReadMap field key) k =
    do
      f <- readFile $ show field
      case f of
           Right val => do
             putStrLn $ "- Read " ++ show field ++ ": " ++ trim val
             k (deserialize field val) s
           Left _ => do
             putStrLn $ "Error reading file for " ++ show field 
             k (defVal field) s
             
  handle s (WriteMap field key val) k =
    do
      putStrLn $ "- Write " ++ show field ++ " = " ++ serialize field val 
      writeFile (show field) (serialize field val)
      k () s

