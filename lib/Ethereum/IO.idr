module Ethereum.IO

import public Effects
import public Ethereum.Types
import public Ethereum.Store
import public Ethereum.Ether
import public Ethereum.Environment

%default total

Handler EnvRules IO where
  handle state@(MkEnv c _ _) Self     k = k c state
  handle state@(MkEnv _ s _) Sender   k = k s state
  handle state@(MkEnv _ _ o) Origin   k = k o state
  handle state RemainingGas           k = k 100 state
  handle state TimeStamp              k = k 1453299096 state
  handle state@(MkEnv _ _ o) Coinbase k = k o state

Handler EtherRules IO where
  handle state@(MkEth v _ _ _) Value           k = k v   state
  handle state@(MkEth _ b _ _) ContractBalance k = k b   state
  handle state                 (Balance a)     k = k 100 state --Dummy value
  handle (MkEth v b t s)       (Save a)        k = do putStrLn $ "- Save " ++ show a ++ " wei"
                                                      k () (MkEth v b t (s+a))
  handle (MkEth v b t s)       (Send a r) k = do putStrLn $ "- " ++ show a ++ " wei => " ++ show r
                                                 k () (MkEth v b (t+a) s)

{-
Handler Store IO where
  handle s (Read field)     k =
    do
      f <- readFile $ show field
      case f of
           Right val => do
             putStrLn $ "- " ++ show (name field) ++ ": " ++ trim val
             k (deserialize field val) s
           Left _ => do
             putStrLn $ "- " ++ name field ++ ": Default " ++ serialize field (defVal field)
             k (defVal field) s
                  
  handle s (Write field val) k =
    do
      putStrLn $ "- " ++ name field ++ " = " ++ serialize field val 
      writeFile (show field) (serialize field val ++ "\n")
      k () s

  handle s (ReadMap field key) k =
    do
      f <- readFile $ show field ++ serialize field key
      case f of
           Right val => do
             putStrLn $ "- " ++ name field ++ "[" ++ serialize field key ++ "]: " ++ trim val
             k (deserialize field val) s
           Left _ => do
             putStrLn $ "- " ++ name field  ++ "[" ++ serialize field key ++ "]: Default " ++ serialize field (defVal field)
             k (defVal field) s
             
  handle s (WriteMap field key val) k =
    do
      putStrLn $ "- " ++ name field ++ "["++ serialize field key ++"] = " ++ serialize field val 
      writeFile (show field ++ serialize field key) (serialize field val ++ "\n")
      k () s

-}
