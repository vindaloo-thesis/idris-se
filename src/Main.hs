module Main where

import Idris.Core.TT
import Idris.AbsSyntax
import Idris.ElabDecls
import Idris.REPL

import IRTS.Compiler
import IRTS.CodegenPHP

import System.Environment
import System.Exit
import Control.Monad

import Paths_idris_se

data Opts = Opts { inputs :: [FilePath],
                   interface :: Bool,
                   output :: FilePath }

showUsage = do putStrLn "Usage: idris-php <ibc-files> [-o <output-file>]"
               exitWith ExitSuccess

getOpts :: IO Opts
getOpts = do xs <- getArgs
             return $ process (Opts [] False "a.php") xs
  where
    process opts ("-o":o:xs) = process (opts { output = o }) xs
    process opts ("--interface":xs) = process (opts { interface = True }) xs
    process opts (x:xs) = process (opts { inputs = x:inputs opts }) xs
    process opts [] = opts

c_main :: Opts -> Idris ()
c_main opts = do elabPrims
                 loadInputs (inputs opts) Nothing
                 mainProg <- if interface opts
                                then liftM Just elabMain
                                else return Nothing
                 ir <- compile (Via "c") (output opts) mainProg
                 runIO $ codegenPHP ir

main :: IO ()
main = do opts <- getOpts
          if (null (inputs opts))
             then showUsage
             else runMain (c_main opts)


