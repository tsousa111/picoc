module PicoUnparser where

import PicoTypes
import Prelude hiding (EQ, GT, LT)

---------------------------
-- unparse : AST to text --
-- pretty printing       --
---------------------------
instance Show PicoC where
    show = unparse

unparse :: PicoC -> String
unparse (PicoC x) = unparseListInst x

unparseListInst :: [Inst] -> String
unparseListInst = concatMap unparseInst

unparseInst (Attrib x y) = x ++ " = " ++ unparseExp y ++ ";\n"
unparseInst (While x y) = "while (" ++ unparseExp x ++ ") " ++ unparseCBlock y
unparseInst (ITE x y []) = "if (" ++ unparseExp x ++ ") then " ++ unparseCBlock y
unparseInst (ITE x y z) = "if (" ++ unparseExp x ++ ") then " ++ unparseCBlock y ++ " else " ++ unparseCBlock z
unparseInst (Return x) = "return " ++ unparseExp x ++ ";\n"
unparseInst (Print x) = "print(" ++ x ++ ");\n"

unparseExp (Const x) = show x
unparseExp (Var x) = x
unparseExp TRUE = "TRUE"
unparseExp FALSE = "FALSE"
unparseExp (AND x y) = "(" ++ unparseExp x ++ " && " ++ unparseExp y ++ ")"
unparseExp (OR x y) = "(" ++ unparseExp x ++ " || " ++ unparseExp y ++ ")"
unparseExp (Add x y) = "(" ++ unparseExp x ++ " + " ++ unparseExp y ++ ")"
unparseExp (Sub x y) = "(" ++ unparseExp x ++ " - " ++ unparseExp y ++ ")"
unparseExp (Mult x y) = "(" ++ unparseExp x ++ " * " ++ unparseExp y ++ ")"
unparseExp (Div x y) = "(" ++ unparseExp x ++ " / " ++ unparseExp y ++ ")"
unparseExp (Neg x) = "-" ++ "(" ++ unparseExp x ++ ")"
unparseExp (LT x y) = "(" ++ unparseExp x ++ " < " ++ unparseExp y ++ ")"
unparseExp (GT x y) = "(" ++ unparseExp x ++ " > " ++ unparseExp y ++ ")"
unparseExp (LET x y) = "(" ++ unparseExp x ++ " <= " ++ unparseExp y ++ ")"
unparseExp (GET x y) = "(" ++ unparseExp x ++ " >= " ++ unparseExp y ++ ")"
unparseExp (DIFF x y) = "(" ++ unparseExp x ++ " != " ++ unparseExp y ++ ")"
unparseExp (EQ x y) = "(" ++ unparseExp x ++ " == " ++ unparseExp y ++ ")"
unparseExp (Not x) = "!" ++ "(" ++ unparseExp x ++ ")"

unparseCBlock :: CBlock -> String
unparseCBlock x = "{\n" ++ unparseListInst x ++ "}\n"
