module IRTS.CodegenSe(codegenSe) where

import IRTS.CodegenCommon
import IRTS.Lang
import IRTS.Simplified
import Idris.Core.TT

import Data.Maybe
import Data.Char
import Data.List
import Data.Text (unpack, pack, splitOn)
import qualified Data.Text as T
import Numeric

codegenSe :: CodeGenerator
codegenSe ci = do let out = concatMap doCodegen (simpleDecls ci)
                      exports = concat (concatMap cgExport (exportDecls ci))
                  writeFile (outputFile ci) ("\n" ++ helpers ++ "\n" ++
                                                    out ++ "\n" ++
                                                    exports ++ "\n")

helpers = "def idris_Prelude_46_Nat_46_toIntNat_58_toIntNat_39__58_0(loc0, loc1, loc2): #Prelude.Nat.toIntNat:toIntNat':0\n  return loc1\n"

sename :: Name -> String
sename n = "idris_" ++ concatMap sechar (showCG n)
  where sechar x | isAlpha x || isDigit x = [x]
                 | otherwise = "_" ++ show (fromEnum x) ++ "_"

var :: Name -> String
var n = sename n


wloc :: Int -> String
wloc i = "v" ++ show i

indent :: Int -> String
indent ind = take (ind*2) $ repeat ' '

doCodegen :: (Name, SDecl) -> String
--doCodegen (n, SFun _ args i def) = show n ++ ", [" ++ showSep ", " (map show args) ++ "]" ++ show i ++ "\n" -- cgFun n args def
doCodegen (n, SFun _ args i def) = cgFun n args def

wcgArgs :: Int -> String
wcgArgs n = showSep ", " (map wloc [0..(n-1)])

--TODO: Real export interface
cgExport :: ExportIFace -> [String]
cgExport (Export _ffiName _fileName es) = ["#exports:\n"] ++ map cgExportDecl es ++ ["#/exports\n"]

cgExportDecl :: Export -> String
cgExportDecl (ExportFun fn (FStr en) (FIO ret) argTys) = "#exported: " ++ show fn ++ "\n"
cgExportDecl (ExportFun fn (FStr en) ret argTys) = "#exported: " ++ show fn ++ " " ++ en ++ "\n" ++
  "def " ++ en ++ "(" ++ wcgArgs (length argTys) ++"): #" ++ show (length argTys) ++ "\n" ++
  "  " ++ sename fn ++ "("++ wcgArgs (length argTys) ++")\n" ++
  "  return out[1]\n\n"
--  "  if out[0] == 0:\n    return 255\n  return out[1]\n\n"
cgExportDecl _ = ""  -- ignore everything else. Like Data.


prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys


shouldSkip :: Name -> Bool
shouldSkip n@(NS r ns) = any (\x -> elem (str x) [
  -- Skipped namespaces
  "__prim", "prim" , "Effects"
  ]) ns || elem (showCG n) [
  -- Skipped functions
  "Prelude.Bool.&&",
  "call__IO",
  "Prelude.Bool.ifThenElse",
  "Prelude.Classes.intToBool",
  "mkForeignPrim",
  "Force",
  "Void_case",
  "Void_elim"
  ]
shouldSkip n@(SN _) = not $ elem (showCG n) [
  --"Prelude.Nat.toIntNat:toIntNat':0"
  ]
shouldSkip (UN n) = not $ elem (str n) [
  --Included "special" (really user defined) functions
  "io_return",
  "io_bind"
  ]
shouldSkip (MN _ _) = True -- APPLY, EVAL
shouldSkip n = True --False --let s = showCG n in prefix s "Effects" --False -- Hitt på nåt. let s = showCG n in isInfixOf "Ethereum" s || isInfixOf "Prelude" s

isNativeEff :: Name -> Bool -- TODOL Determine what order namespaces come in to make this precise & pretty
isNativeEff (NS _ ns) = case map unpack (reverse ns) of --TODO: Env, Eth
                          ("Ethereum":"Store":_) -> True
                          _ -> False --any (\x -> elem (str x) ["Store"]) ns && any (\x -> elem (str x) ["Ethereum"]) ns
isNativeEff _ = False



cgFun :: Name -> [Name] -> SExp -> String
cgFun n@(NS n' ns) args def
  | shouldSkip n =  "#"++ showCG n ++"\n"
  | isNativeEff n = cgSig n args
                    ++ cgNative n'
  | otherwise     = cgSig n args
                    ++ argVars (length args)
                    ++ cgBody 1 doRet def ++ "\n\n"
  where doRet :: Int -> String -> String -- Return the calculated expression
        doRet ind "out" = "\n"
        doRet ind str = "out = " ++ str ++ "\n"
        --  | head str == '[' && elem ',' str = "retVal = " ++ str ++ "\n" ++ indent ind ++ "return retVal\n"
        --  | otherwise                     = "return " ++ str ++ "\n"
        cgSig :: Name -> [Name] -> String
        cgSig n args = "macro " ++ sename n ++ "("
                       ++ cgArgs (length args) ++ "): #"++ showCG n ++"\n"
        --TODO: This is not so nice. should call primitives directly here I guess.
        cgNative :: Name -> String
        cgNative (UN n) =
                   case map unpack (n:ns) of
                     ("write":"MapField":_) ->
                       "  idris_Ethereum_46_EIO_46_prim_95__95_writeMap($a0[0], $a1, $a2)\n"
                       ++ "  out = 0\n\n"
                     ("read":"MapField":_) -> "  idris_Ethereum_46_EIO_46_prim_95__95_readMap($a0[0], $a1)\n"
                             ++  "  rv = out\n  out = [0, rv]\n\n"
                     x       -> "error('unimplemented native', " ++ show x ++ ")\n"
-- cgBody converts the SExp into a chunk of se which calculates the result
-- of an expression, then runs the function on the resulting bit of code.
--
-- We do it this way because we might calculate an expression in a deeply nested
-- case statement, or inside a let, etc, so the assignment/return of the calculated
-- expression itself may happen quite deeply.
        argVars :: Int -> String
        argVars n = concatMap argAss [0..(n-1)]

        argAss :: Int -> String
        argAss i = indent 1 ++ loc i ++ " = " ++ argVar i ++ "\n"

        argVar i = "$a" ++ show i


        loc :: Int -> String
        loc i = "v_" ++ qhash i (showCG n)

        -- Simple but effective string hashing...
        -- From src/Idris/Elab/Clause.hs
        qhash :: Int -> String -> String
        qhash hash [] = showHex (abs hash `mod` 0xffffffff) ""
        qhash hash (x:xs) = qhash (hash * 33 + fromEnum x) xs

        cgArgs :: Int -> String
        --cgArgs n = showSep ", " (map loc [0..(n-1)])
        cgArgs n = showSep ", " (map argVar [0..(n-1)])

        cgBody :: Int -> (Int -> String -> String) -> SExp -> String
        cgBody ind ret (SV (Glob n)) = indent ind ++ (ret ind $ sename n ++ "()")
        cgBody ind ret (SV (Loc i)) = indent ind ++ (ret ind $ loc i)
        cgBody ind ret (SApp _ f args)
        -- TODO: Case ethereumprim and generate proper serpent calls immediatly, don't generate our own prim__functions. This avoids unnecessary overhead of the prim__ functions.
          -- | otherwise        = indent ind ++ ret ind (sename f ++ "(" ++
          --                                 showSep ", " (map cgVar args) ++ ")")
          | shouldSkip f     = let res = ret ind "out" in
                                   if dropWhile (flip elem [' ', '\n']) res == ""
                                      then indent ind ++ "out = [0]\n"
                                      else indent ind ++ res -- ++"#'"++res++"'"
          | otherwise        = indent ind ++ (sename f ++ "(" ++ showSep ", " (map cgVar args) ++ ")") ++ "\n"
                            ++ indent ind ++ ret ind "out"
        cgBody ind ret (SLet (Loc i) v sc)
           = cgBody ind (\_ x -> loc i ++ " = " ++ x ++ "\n") v ++
             cgBody ind ret sc
        cgBody ind ret (SUpdate n e)
           = cgBody ind ret e
        cgBody ind ret (SProj e i)
           = indent ind ++ (ret ind $ cgVar e ++ "[" ++ show (i + 1) ++ "]")
        cgBody ind ret (SCon _ t n args)
           = indent ind ++ (ret ind $ "[" ++ showSep ","
                      (show t : (map cgVar args)) ++ "]")
        cgBody ind ret (SCase _ e alts) = cgBody ind ret (SChkCase e alts)
        cgBody ind ret (SChkCase e (a:alts))
           = let scr = cgVar e
                 scrvar = if any conCase alts || conCase a then scr ++ "[0]" else scr in
                 (cgAlt ind ret scr scrvar "if" a) ++ showSep "\n" (map (cgAlt ind ret scr scrvar "elif") alts) ++ "\n"
          where conCase (SConCase _ _ _ _ _) = True
                conCase _ = False
        cgBody ind ret (SConst c) = indent ind ++ (ret ind $ cgConst c)
        cgBody ind ret (SOp (LExternal (NS (UN t) _)) args) = indent ind ++ cgEthereumPrim ind ret (unpack t) (map cgVar args)
        cgBody ind ret (SOp op args) = indent ind ++ (ret ind $ cgOp op (map cgVar args))
        cgBody ind ret SNothing = indent ind ++ (ret ind "0 #Nothing")
        cgBody ind ret (SError x) = indent ind ++ (ret ind $ "error( " ++ show x ++ ")")
        cgBody ind ret (SForeign desc1 desc2 args) = indent ind ++ "ERROR('Unhandled foreign function " ++ show desc1 ++ ", "++ show desc2 ++ ")"

        cgFArgs :: [(FDesc,LVar)] -> String
        cgFArgs []   = ""
        cgFArgs args = "(" ++ intercalate "," (map (cgVar . snd) args) ++ ")"

        cgAlt :: Int -> (Int -> String -> String) -> String -> String -> String -> SAlt -> String
        cgAlt ind ret scr scrvar f (SConstCase t exp)
           = indent ind ++ (f ++ " " ++ scrvar ++ " == " ++ show t ++ ":\n" ++ cgBody (ind+1) ret exp)
        cgAlt ind ret scr scrvar f (SDefaultCase exp)
           = indent ind ++ (f ++ " True:\n" ++ cgBody (ind+1) ret exp)
        cgAlt ind ret scr scrvar f (SConCase lv t n args exp)
           = indent ind ++ (f ++ " " ++ scrvar ++ " == " ++ show t ++ ":\n"
                     ++ project 1 lv args ++ "\n" ++ cgBody (ind+1) ret exp)
           where project i v [] = "" -- indent (ind+1) ++ "#empty project"
                 project i v (n : ns) = indent (ind+1) ++ (loc v ++ " = " ++ scr ++ "[" ++ show i ++ "]\n"
                                          ++ project (i + 1) (v + 1) ns)

        cgVar :: LVar -> String
        cgVar (Loc i) = loc i
        cgVar (Glob n) = var n
        {-
cgFun n@(NS _ _) _ _ =  "#NS " ++ show n++ "\n"
cgFun n@(SN _) _ _ =  "#SN " ++ show n++ "\n"
cgFun n@(UN _) _ _ =  "#UN " ++ show n++ "\n"
cgFun n@(MN _ _) _ _ =  "#MN " ++ show n++ "\n"
cgFun n@(SymRef _) _ _ =  "#SymRef " ++ show n++ "\n"
-}
cgFun n _ _= ""
cgConst :: Const -> String
cgConst (I i) = show i
cgConst (Ch i) = show (ord i) -- Treat Char as ints, because Se treats them as Strings...
cgConst (BI i) = show i
cgConst (Str s) = show s
cgConst TheWorld = "0 #TheWorld"
cgConst x | isTypeConst x = "0 #TypeConst"
cgConst x = error $ "Constant " ++ show x ++ " not compilable yet"

cgOp :: PrimFn -> [String] -> String
cgOp (LPlus (ATInt _)) [l, r]
     = "(" ++ l ++ " + " ++ r ++ ")"
cgOp (LMinus (ATInt _)) [l, r]
     = "(" ++ l ++ " - " ++ r ++ ")"
cgOp (LTimes (ATInt _)) [l, r]
     = "(" ++ l ++ " * " ++ r ++ ")"
cgOp (LEq (ATInt _)) [l, r]
     = "(" ++ l ++ " == " ++ r ++ ")"
cgOp (LSLt (ATInt _)) [l, r]
     = "(" ++ l ++ " < " ++ r ++ ")"
cgOp (LSLe (ATInt _)) [l, r]
     = "(" ++ l ++ " <= " ++ r ++ ")"
cgOp (LSGt (ATInt _)) [l, r]
     = "(" ++ l ++ " > " ++ r ++ ")"
cgOp (LSGe (ATInt _)) [l, r]
     = "(" ++ l ++ " >= " ++ r ++ ")"
cgOp (LSExt _ _) [x] = x
cgOp op exps = "0 #error(\"OPERATOR " ++ show op ++ " NOT IMPLEMENTED!!!!\")"

cgEthereumPrim :: Int -> (Int -> String -> String) -> String -> [String] -> String
cgEthereumPrim ind ret "prim__value"        args = ret ind "msg.value"
cgEthereumPrim ind ret "prim__selfbalance"  args = ret ind $ "self.balance"
cgEthereumPrim ind ret "prim__balance"      args = ret ind $ head args ++ ".balance"
cgEthereumPrim ind ret "prim__send"         args = ret ind $ "send(0, " ++ head args ++ ", " ++ (args !! 1) ++ ")"
cgEthereumPrim ind ret "prim__remainingGas" args = ret ind $ "msg.gas"
cgEthereumPrim ind ret "prim__timestamp"    args = ret ind $ "block.timestamp"
cgEthereumPrim ind ret "prim__coinbase"     args = ret ind $ "block.coinbase"
cgEthereumPrim ind ret "prim__self"         args = ret ind $ "self"
cgEthereumPrim ind ret "prim__sender"       args = ret ind $ "msg.sender"
cgEthereumPrim ind ret "prim__origin"       args = ret ind $ "tx.origin"
cgEthereumPrim ind ret "prim__gasprice"     args = ret ind $ "tx.gasprice"
cgEthereumPrim ind ret "prim__prevhash"     args = ret ind $ "block.prevhash"
cgEthereumPrim ind ret "prim__difficulty"   args = ret ind $ "block.difficulty"
cgEthereumPrim ind ret "prim__blocknumber"  args = ret ind $ "block.number"
cgEthereumPrim ind ret "prim__gaslimit"     args = ret ind $ "block.gaslimit"
cgEthereumPrim ind ret "prim__read"            args = ret ind $ "self.storage[" ++ head args ++ "]"
cgEthereumPrim ind ret "prim__write"           args = "self.storage[" ++ head args ++ "] = " ++ (args !! 1) ++ "\n" ++ indent ind ++ ret ind "0"
cgEthereumPrim ind ret "prim__readMap"         args =
  "mk = 'idr_' + " ++ head args ++ " + '_' + " ++ (args !! 1) ++ "\n" ++
  indent ind ++ (ret ind "self.storage[mk]")
cgEthereumPrim ind ret "prim__writeMap"         args =
  "mk = 'idr_' + " ++ head args ++ " + '_' + " ++ (args !! 1) ++ "\n" ++
   indent ind ++ "self.storage [mk] = " ++ (args !! 2)++"\n" ++ indent ind ++ ret ind "0"

cgEthereumPrim ind ret n _ =  "ERROR('Unimplemented cgEthereumPrim\')"

