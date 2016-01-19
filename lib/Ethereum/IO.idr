module Ethereum.IO

import Effects
import Ethereum.Types
import Ethereum.GeneralStore
import Ethereum.Ether

instance Handler EtherRules IO where
  handle (MkS v t s) Value    k = k v (MkS v t s)

  handle (MkS v t s) ContractAddress k = k 0x00000000000000000000000000000000deadbeef (MkS v t s)

  handle (MkS v t s) (Balance a) k = k 100 (MkS v t s) -- TODO: Change this. Balance should be *read*.

  handle (MkS v t s) (Save a) k = do putStrLn $ "- Saved " ++ show a
                                     k () (MkS v t (s+a))

  handle (MkS v t s) (Send a r) k = do putStrLn $ "- Sent  " ++ show a ++ " to " ++ show r
                                       k () (MkS v (t+a) s)

  handle (MkS v t s) Sender k   = k 0x00cf7667b8dd4ece1728ef7809bc844a1356aadf (MkS v t s)

instance Handler Store IO where
  handle s (Read field)     k =
    do
      f <- readFile $ show field
      case f of
           Right val => do
             putStrLn $ "- Read " ++ show field ++ ": " ++ trim "val"
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
             putStrLn $ "- Read " ++ show field ++ ": " ++ trim "val"
             k (deserialize field val) s
           Left _ => do
             putStrLn $ "Error reading file for " ++ show field 
             k (defVal field) s
             
  handle s (WriteMap field key val) k =
    do
      putStrLn $ "- Write " ++ show field ++ " = " ++ serialize field val 
      writeFile (show field) (serialize field val)
      k () s

