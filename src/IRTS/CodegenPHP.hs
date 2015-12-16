module IRTS.CodegenPHP(codegenPHP) where

import IRTS.CodegenCommon
import IRTS.Lang
import IRTS.Simplified
import Idris.Core.TT

import Data.Maybe
import Data.Char
import Data.List

codegenPHP :: CodeGenerator
codegenPHP ci = do let out = concatMap doCodegen (simpleDecls ci)
                       exports = concat (concatMap cgExport (exportDecls ci))
                   writeFile (outputFile ci) ("\n" ++ helpers ++ "\n" ++
                                                        out ++ "\n#exports:\n" ++
                                                        exports ++ "\n#/exports" ++
                                                        show (length (exportDecls ci)) ++
                                                        start ++ "\n" ++
                                              "\n\n")

start = ""

helpers = ""

phpname :: Name -> String
phpname n = "idris_" ++ concatMap phpchar (showCG n)
  where phpchar x | isAlpha x || isDigit x = [x]
                  | otherwise = "_" ++ show (fromEnum x) ++ "_"

var :: Name -> String
var n = phpname n

loc :: Int -> String
loc i = "loc" ++ show i

indent :: Int -> String
indent ind = take (ind*2) $ repeat ' '

doCodegen :: (Name, SDecl) -> String
doCodegen (n, SFun _ args i def) = cgFun n args def

--EXPORTS START
cgExport :: ExportIFace -> [String]
cgExport (Export _ffiName _fileName es) = map cgExportDecl es

cgExportDecl :: Export -> String
cgExportDecl (ExportFun fn (FStr en) (FIO ret) argTys)
    = cgExportFun fn en (length argTys)
cgExportDecl _ = ""  -- ignore everything else. Like Data.
-- Example: ExportFun Main.exports, greet (FStr "greet") (FIO (FCon PyUnit)) []

cgExportFun :: Name -> String -> Int -> String
cgExportFun fn en argCnt
    = ("#export: " ++ show fn)
    {-
    $+$ text "def" <+> cgApp (text en) (map text args) <> colon
    $+$ indent (
        cgApp
            (cgName (sMN 0 "APPLY"))
            [ cgApp (cgName fn)
                $ map text args
            , text "World"
            ]
    )
    $+$ text ""
  where
    args = ["arg" ++ show i | i <- [1..argCnt]]
    -}

--EXPORTS END

shouldSkip :: Name -> Bool
shouldSkip n@(NS _ ns) = any (\x -> elem (str x) [
  -- Skipped namespaces
  "__prim", "prim" --, "Ether"
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
shouldSkip (SN n) = True --not $ elem (str n) [
shouldSkip (UN n) = not $ elem (str n) [
  --Included "special" (really user defined) functions
  "io_return"
  ]
shouldSkip n = False -- Hitt på nåt. let s = showCG n in isInfixOf "Ethereum" s || isInfixOf "Prelude" s

cgFun :: Name -> [Name] -> SExp -> String
cgFun n args def
  | shouldSkip n = "#"++ showCG n ++"\n"
  | otherwise    = "def " ++ phpname n ++ "("
                    ++ showSep ", " (map (loc . fst) (zip [0..] args)) ++ "): #"++ showCG n ++"\n"
                    ++ cgBody 2 doRet def ++ "\n\n"
  where doRet :: Int -> String -> String -- Return the calculated expression
        doRet ind str = "retVal = " ++ str ++ "\n" ++ indent ind ++ "return retVal\n"

etherApp :: Name -> [String] -> String
etherApp (NS (UN (t)) _) args = eApp (str t) args where
  --eApp "save" _ = "[2,[0],[4]] #save "++ head args ++ "\n"
  --eApp "balance" _ = "self.s.block.balance(" ++ args !! 4 ++ ")\n"
  --eApp "balance" _ = args !! 4 ++ ".balance\n"
  --eApp "contractAddress" _ = "self\n"
  --eApp "sender" _ = "msg.sender"
  --eApp "send" _ = "send(" ++ (args !! 4) ++ ", " ++ (args !! 5) ++ ")\n"
  eApp f  args = "UNHANDLED EVM FUNCTION " ++ f ++ "(" ++ showSep ", " args ++ ")\n"

isEthereumPrim :: Name -> Bool
isEthereumPrim (NS f ns) = any (\x -> str x == "Ether") ns
isEthereumPrim n         = False

-- cgBody converts the SExp into a chunk of php which calculates the result
-- of an expression, then runs the function on the resulting bit of code.
--
-- We do it this way because we might calculate an expression in a deeply nested
-- case statement, or inside a let, etc, so the assignment/return of the calculated
-- expression itself may happen quite deeply.

cgBody :: Int -> (Int -> String -> String) -> SExp -> String
cgBody ind ret (SV (Glob n)) = indent ind ++ (ret ind $ phpname n ++ "()")
cgBody ind ret (SV (Loc i)) = indent ind ++ (ret ind $ loc i)
cgBody ind ret (SApp _ f args)
--  | isEthereumPrim f = indent ind ++ ret ind (etherApp f (map cgVar args))
  | otherwise        = indent ind ++ ret ind ("self." ++ phpname f ++ "(" ++
                                   showSep ", " (map cgVar args) ++ ")")
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
cgBody ind ret (SOp op args) = indent ind ++ (ret ind $ cgOp op (map cgVar args))
cgBody ind ret SNothing = indent ind ++ (ret ind "0 #Nothing")
cgBody ind ret (SError x) = indent ind ++ (ret ind $ "error( " ++ show x ++ ")")
cgBody ind ret (SForeign desc1 desc2 args) = indent ind ++ cgFDesc ind ret desc1 desc2 args


cgFDesc :: Int -> (Int -> String -> String) -> FDesc -> FDesc -> [(FDesc,LVar)] -> String
cgFDesc ind ret _ (FStr n) args = case n of
                                    "writeVal"   -> cgVarWrite args
                                    "readVal"    -> ret ind $ cgVarRead args
                                    "getBalance" -> ret ind $ (cgVar . snd . head $ args) ++ ".balance"
                                    _            -> ret ind $ n ++ cgFArgs args
cgFDesc ind _   _ fdesc    _    = "ERROR!!! UNIMPLEMENTED CASE OF cgFDesc: " ++ show fdesc

cgFArgs :: [(FDesc,LVar)] -> String
cgFArgs []   = ""
cgFArgs args = "(" ++ intercalate "," (map (cgVar . snd) args) ++ ")"

cgVarRead :: [(FDesc,LVar)] -> String
cgVarRead [(_,var)] = "self.storage[" ++ cgVar var ++ "]"
cgVarRead args      = "ERROR: missing case in cgVarRead" ++ show args

cgVarWrite :: [(FDesc,LVar)] -> String
cgVarWrite [(_,var),(_,val)] = "self.storage[" ++ cgVar var ++ "] = " ++ cgVar val
cgVarWrite args              = "ERROR: missing case in cgVarWrite" ++ show args 

cgAlt :: Int -> (Int -> String -> String) -> String -> String -> String -> SAlt -> String
cgAlt ind ret scr scrvar f (SConstCase t exp)
   = indent ind ++ (f ++ " " ++ scrvar ++ " == " ++ show t ++ ":\n" ++ cgBody (ind+1) ret exp)
cgAlt ind ret scr scrvar f (SDefaultCase exp)
   = indent ind ++ (f ++ " True:\n" ++ cgBody (ind+1) ret exp)
cgAlt ind ret scr scrvar f (SConCase lv t n args exp)
   = indent ind ++ (f ++ " " ++ scrvar ++ " == " ++ show t ++ ":\n"
             ++ project 1 lv args ++ "\n" ++ cgBody (ind+1) ret exp)
   where project i v [] = indent (ind+1) ++ "#empty project"
         project i v (n : ns) = indent (ind+1) ++ (loc v ++ " = " ++ scr ++ "[" ++ show i ++ "]\n"
                                  ++ project (i + 1) (v + 1) ns)

{-
cgAlt :: Int -> (Int -> String -> String) -> String -> String -> String -> SAlt -> String
cgAlt ind ret scr scrvar f (SConstCase t exp)
   = "case " ++ show t ++ ":\n" ++ cgBody ind ret exp
cgAlt ind ret scr scrvar f (SDefaultCase exp) = "default:\n" ++ cgBody ind ret exp
cgAlt ind ret scr scrvar f (SConCase lv t n args exp)
   = "case " ++ show t ++ ":\n"
             ++ project 1 lv args ++ "\n" ++ cgBody ind ret exp
   where project i v [] = ""
         project i v (n : ns) = loc v ++ " = " ++ scr ++ "[" ++ show i ++ "]; "
                                  ++ project (i + 1) (v + 1) ns
                                  -}

cgVar :: LVar -> String
cgVar (Loc i) = loc i
cgVar (Glob n) = var n

cgConst :: Const -> String
cgConst (I i) = show i
cgConst (Ch i) = show (ord i) -- Treat Char as ints, because PHP treats them as Strings...
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
     {-
cgOp LStrEq [l,r] = "(" ++ l ++ " == " ++ r ++ ")"
cgOp LStrRev [x] = "strrev(" ++ x ++ ")"
cgOp LStrLen [x] = "strlen(utf8_decode(" ++ x ++ "))"
cgOp LStrHead [x] = "ord(" ++ x ++ "[0])"
cgOp LStrIndex [x, y] = "ord(" ++ x ++ "[" ++ y ++ "])"
cgOp LStrTail [x] = "substr(" ++ x ++ ", 1)"

cgOp (LIntStr _) [x] = "\"" ++ x ++ "\""
cgOp (LChInt _) [x] = x
cgOp (LIntCh _) [x] = x
cgOp (LSExt _ _) [x] = x
cgOp (LTrunc _ _) [x] = x
cgOp LWriteStr [_,str] = "idris_writeStr(" ++ str ++ ")"
cgOp LReadStr [_] = "idris_readStr()"
cgOp LStrConcat [l,r] = "idris_append(" ++ l ++ ", " ++ r ++ ")"
cgOp LStrCons [l,r] = "idris_append(chr(" ++ l ++ "), " ++ r ++ ")"
cgOp (LStrInt _) [x] = x
-}
cgOp op exps = "0 #error(\"OPERATOR " ++ show op ++ " NOT IMPLEMENTED!!!!\")"
   -- error("Operator " ++ show op ++ " not implemented")



