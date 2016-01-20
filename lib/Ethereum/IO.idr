module Ethereum.IO

import Effects
import Ethereum.Types
import Ethereum.GeneralStore
import Ethereum.Ether
import Ethereum.Environment

instance Handler EnvRules IO where
  handle state@(MkE c _ _) ContractAddress k = k c state
  handle state@(MkE _ s _) Sender          k = k s state
  handle state@(MkE _ _ o) Origin          k = k o state
  handle state RemainingGas                k = k 100 state
  handle state TimeStamp                   k = k 1453299096 state
  handle state@(MkE _ _ o) Coinbase        k = k o state

instance Handler EtherRules IO where
  handle state@(MkS v _) Value           k = k v   state
  handle state@(MkS _ b) ContractBalance k = k b   state
  handle state           (Balance a)     k = k 100 state
  handle (MkS v b)       (Send a r)      k = do putStrLn $ "- Sent  " ++ show a ++ " to " ++ show r
                                                k () (MkS v (b-a))

instance Handler Store IO where
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

