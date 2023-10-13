--- Given Code
--- ==========

module Lib where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IntExp Integer
         | VarExp String
         | LamExp String Exp
         | IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2
                            ++ " else " ++ show e3 ++ ")"
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (AppExp f e)     = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s)       = "VarExp " ++ show s
ctorShow (IntExp i)       = "IntExp " ++ show i
ctorShow (LamExp x e)     = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) = "IfExp (" ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ") ("
                                ++ ctorShow e3 ++ ")"
ctorShow (OpExp op e1 e2) = "OpExp " ++ show op ++ " ("
                                ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ")"
ctorShow (AppExp f e)     = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

factk :: Integer -> (Integer -> t) -> t
factk n k
  | n <= 0    = k 1
  | otherwise = factk (n - 1) (\res -> k (n * res))

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk [x] ev od  | even x =    ev x
                    | otherwise = od x

evenoddk (x:xs) ev od | even x =    evenoddk xs (\val -> ev $ val + x) od
                      | otherwise = evenoddk xs ev                     (\val -> od $ val + x)


--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`

isSimple :: Exp -> Bool
isSimple (VarExp _     ) = True
isSimple (IntExp _     ) = True
isSimple (AppExp _  _  ) = False
isSimple (IfExp  x  y z) = isSimple x && isSimple y && isSimple z
isSimple (OpExp  op x y) = isSimple x && isSimple y

--- ### Define `cpsExp` - Overview


cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)


--- #### Define `cpsExp` for Integer and Variable Expressions
cpsExp (VarExp x     ) con val = (AppExp con (VarExp x), val)
cpsExp (IntExp x     ) con val = (AppExp con (IntExp x), val)

--- #### Define `cpsExp` for Application Expressions
cpsExp (AppExp x  y  ) con val | isSimple y = (AppExp (AppExp x y) $ con, val)
                               | otherwise  = cpsExp y (LamExp v (AppExp (AppExp x (VarExp v)) con)) val
                               where (v, val') = gensym val

--- #### Define `cpsExp` for Operator Expressions
cpsExp (OpExp  op x y) con val =
    case (isSimple x, isSimple y) of
        (True, True)   -> (AppExp con (OpExp op x y), val)
        (False, True)  -> cpsExp x (LamExp v (AppExp con (OpExp op (VarExp v) y))) val'
                        where
                            (v, val') = gensym val
        (True, False)  -> cpsExp y (LamExp v (AppExp con (OpExp op x (VarExp v)))) val'
                        where
                            (v, val') = gensym val
        (False, False) -> cpsExp x (LamExp v1 ce2) val'''
                        where
                            (v1, val') = gensym val
                            (v2, val'') = gensym val'
                            base = LamExp v2 (AppExp con (OpExp op (VarExp v1) (VarExp v2)))
                            (ce2, val''') = cpsExp y base val''

--- #### Define `cpsExp` for If Expressions
cpsExp (IfExp  x  y z) con val  | isSimple x = (IfExp x newy newz, val)
                                | otherwise  = cpsExp x (LamExp othertemp (IfExp (VarExp othertemp) newy newz)) val1
                                where (othertemp, val1) = gensym val
                                      (newy, _) = cpsExp y con val
                                      (newz, _) = cpsExp z con val

--- ### Define `cpsDecl`
cpsDecl :: Stmt -> Stmt
cpsDecl (Decl f xs e) = Decl (f) (xs ++ ["k"]) con
                        where (con, _) = (cpsExp e (VarExp "k") 1)

