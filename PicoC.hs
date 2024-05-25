{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module PicoC where

import Debug.Trace
import Data.Char
import Data.Data
import Data.Generics.Zipper
import Data.Maybe
import Library.StrategicData (StrategicData)
import Library.Ztrategic
import Parser
import Test.QuickCheck
import Text.Read
import Prelude hiding (EQ, GT, LT, (<$>), (<*>), (<|>))

instance StrategicData PicoC
instance (StrategicData a) => StrategicData [a]

data PicoC = PicoC [Inst]
    deriving (Data, Eq, Show)

data Inst
    = Attrib !String !Exp
    | While !Exp !CBlock
    | ITE !Exp !CBlock !CBlock
    | Return !Exp
    | Print !String
    deriving (Show, Data, Eq)

type CBlock = [Inst]
data Exp
    = Const !Int
    | TRUE
    | FALSE
    | Var !String
    | Add !Exp !Exp
    | Sub !Exp !Exp
    | Mult !Exp !Exp
    | Div !Exp !Exp
    | Neg !Exp
    | AND !Exp !Exp
    | OR !Exp !Exp
    | LT !Exp !Exp
    | GT !Exp !Exp
    | EQ !Exp !Exp
    | Not !Exp
    deriving (Show, Data, Eq)

-- NOTE:
-- TPC (aula de 22/2) - entrega 7/3
-- completar o parser | done
-- estender o parser para permitir outros operadores | done
-- (úteis nas condições do While e IFE)
--       por exemplo em Haskell/C/Java é possível
--            ghci> 2 + 3 > 5
--            False
--       logo a adição tem mais prioridade que > ...
-- completar a função unparse | done
-- melhorar a função opt para fazer todas as optimizacoes | done?
-- testar a propriedade definida pela função prop | done
--   prop :: PicoC -> Bool
--   prop ast = ast == parser (unparse ast)
-- (precisam usar deriving Eq nos datas types)

-- NOTE: isto serve para poder expandir a precedência das expressões
-- sem ter que trocar as ocorrencias da pExp

-- FIX: make this the right precedences
-- https://en.cppreference.com/w/c/language/operator_precedence
pExp = pExp5

pExp5 :: Parser Exp
pExp5 = f <$> pExp4 <*> token' "||" <*> pExp5
     <|> pExp4
  where
    f x _ = OR x

pExp4 :: Parser Exp
pExp4 = f <$> pExp3 <*> token' "&&" <*> pExp4
     <|> pExp3
  where
    f x _ = AND x

pExp3 :: Parser Exp
pExp3 = f <$> pExp2 <*> token' "==" <*> pExp3
     <|> pExp2
  where
    f x _ = EQ x

pExp2 :: Parser Exp
pExp2 = f <$> pExp1 <*> token' "<" <*> pExp2
     <|> g <$> pExp1 <*> token' ">" <*> pExp2
     <|> pExp1
  where
    f x _ = LT x
    g x _ = GT x

pExp1 :: Parser Exp
pExp1 = f <$> pExp0 <*> symbol' '+' <*> pExp1
     <|> g <$> pExp0 <*> symbol' '-' <*> pExp1
     <|> pExp0
  where
    f x _ = Add x
    g x _ = Sub x

pExp0 :: Parser Exp
pExp0 = f <$> pFactor <*> symbol' '*' <*> pExp0
     <|> g <$> pFactor <*> symbol' '/' <*> pExp0
     <|> pFactor
  where
    f x _ = Mult x
    g x _ = Div x

pFactor = f <$> pInt
       <|> g <$> pNames
       <|> h <$> token' "TRUE"
       <|> i <$> token' "FALSE"
       <|> j <$> symbol' '-' <*> pFactor
       <|> k <$> symbol' '!' <*> pFactor
       <|> l <$> symbol' '(' <*> pExp <*> symbol' ')'
  where
    f s = Const $ case readMaybe s :: Maybe Int of
        Just x -> x
        Nothing -> error "Not a number"
    g = Var
    h _ = TRUE
    i _ = FALSE
    j _ = Neg
    k _ = Not
    l _ x _ = x

pAtrib :: Parser Inst
pAtrib = f <$> oneOrMore (satisfy' isAlphaNum) <*> symbol' '=' <*> pExp <*> symbol' ';'
  where
    f x _ z _ = Attrib x z

pCBlock :: Parser CBlock
pCBlock =
    enclosedBy
        (symbol' '{')
        (zeroOrMore pInst)
        (symbol' '}')

pWhile :: Parser Inst
pWhile = f <$> token' "while" <*> pExp <*> pCBlock
  where
    f _ = While

-- NOTE: a prioridade é de baixo para cima
pIf :: Parser Inst
pIf = f <$> token' "if" <*> symbol' '(' <*> pExp <*>symbol' ')' <*> token' "then" <*> pCBlock <*> token' "else" <*> pCBlock
   <|> g <$> token' "if" <*> symbol' '(' <*> pExp <*>symbol' ')' <*> token' "then" <*> pCBlock
  where
    f _ _ x _ _ y _ z = ITE x y z
    g _ _ x _ _ y = ITE x y []

pReturn :: Parser Inst
pReturn = f <$> token' "return" <*> pExp <*> symbol' ';'
  where
    f _ x _ = Return x

pInst :: Parser Inst
pInst = pIf <|> pReturn <|> pWhile <|> pAtrib

pPicoC :: Parser PicoC
pPicoC = PicoC <$> oneOrMore pInst

picoC :: String -> PicoC
picoC s = fst $ head $ filter ((== "") . snd) (pPicoC s)

---------------------------
-- unparse : AST to text
-- pretty printing
---------------------------
-- instance Show PicoC where
--     show = unparse

-- FIX: fix unparser
unparse :: PicoC -> String
unparse (PicoC x) = unparseListInst x

unparseListInst :: [Inst] -> String
unparseListInst = concatMap unparseInst

unparseInst (Attrib x y) = x ++ " = " ++ unparseExp y ++ ";\n"
unparseInst (While x y) = "while (" ++ unparseExp x ++ ") " ++ unparseCBlock y
unparseInst (ITE x y []) = "if (" ++ unparseExp x ++ ") then " ++ unparseCBlock y
unparseInst (ITE x y z) = "if (" ++ unparseExp x ++ ") then " ++ unparseCBlock y ++ " else " ++ unparseCBlock z

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
unparseExp (Neg x) = "(" ++ "-" ++ unparseExp x ++ ")"
unparseExp (LT x y) = "(" ++ unparseExp x ++ " < " ++ unparseExp y ++ ")"
unparseExp (GT x y) = "(" ++ unparseExp x ++ " > " ++ unparseExp y ++ ")"
unparseExp (EQ x y) = "(" ++ unparseExp x ++ " == " ++ unparseExp y ++ ")"
unparseExp (Not x) = "(" ++ "!" ++ unparseExp x ++ ")"

unparseCBlock :: CBlock -> String
unparseCBlock x = "{\n" ++ unparseListInst x ++ "}\n"

-- NOTE: funcao prop do tpc 22/2
prop :: PicoC -> Bool
prop ast = ast == picoC (unparse ast)

data Types = Int !Int | Bool !Bool
instance Num Types where
    (+) (Int a) (Int b) = Int (a + b)
    (-) (Int a) (Int b) = Int (a - b)
    (*) (Int a) (Int b) = Int (a * b)
    abs (Int a) = Int (abs a)
    signum (Int a) = Int (signum a)
    fromInteger a = Int (fromInteger a)

eval :: Exp -> [(String, Int)] -> Int
eval (Const x) _ = x
eval (Var x) c = case lookup x c of
    Just v -> v
    Nothing -> error "Variable not found"
eval (Add x y) c = eval x c + eval y c
eval (Sub x y) c = eval x c - eval y c
eval (Mult x y) c = eval x c * eval y c
eval (Div x y) c = eval x c `div` eval y c
eval (Neg x) c = -(eval x c)
eval TRUE _ = 1
eval FALSE _ = 0
eval (LT e1 e2) c = if eval e1 c < eval e2 c then 1 else 0
eval (GT e1 e2) c = if eval e1 c > eval e2 c then 1 else 0
eval (EQ e1 e2) c = if eval e1 c == eval e2 c then 1 else 0
eval (AND e1 e2) c = if (eval e1 c /= 0) && (eval e2 c /= 0) then 1 else 0
eval (OR e1 e2) c = if (eval e1 c /= 0) || (eval e2 c /= 0) then 1 else 0
eval (Not e) c = if not (eval e c /= 0) then 1 else 0

opt :: Exp -> Exp
opt (Add (Const 0) x) = opt x
opt (Add x (Const 0)) = opt x
opt (Add (Const x) (Const y)) = Const (x + y)
opt (Neg (Const x)) = Const (-x)
opt (Neg (Neg x)) = opt x
opt (Sub x y) = opt (Add x (Neg y))
opt (Mult (Const 0) x) = Const 0
opt (Mult x (Const 0)) = Const 0
opt (Mult (Const 1) x) = opt x
opt (Mult x (Const 1)) = opt x
opt (Mult x y) = Mult (opt x) (opt y)
opt (Div x (Const 1)) = opt x
opt (Div (Const 0) _) = Const 0
opt (Div x y) = Div (opt x) (opt y)
opt x = x

optInst :: Inst -> Inst
optInst (Attrib x y) = Attrib x (opt y)
optInst (While x y) = While (opt x) (map optInst y)
optInst (ITE x y z) = ITE (opt x) (map optInst y) (map optInst z)

-- NOTE: 2 ex5
arithmeticOptRec :: PicoC -> PicoC
arithmeticOptRec (PicoC x) = PicoC (map optInst x)

-- NOTE: 4 ex 2
etiquetaVars :: PicoC -> PicoC
etiquetaVars p =
    let pZipper = toZipper p
        Just newp = applyTP (full_tdTP step) pZipper
        step = idTP `adhocTP` etiquetaUma `adhocTP` etiquetaAtrib
     in fromZipper newp

etiquetaUma :: Exp -> Maybe Exp
etiquetaUma (Var x) = Just (Var ("v_" ++ x))
etiquetaUma x = Just x

-- NOTE: 4 ex 5
etiquetaAtrib :: Inst -> Maybe Inst
etiquetaAtrib (Attrib x y) = Just (Attrib ("v_" ++ x) y)
etiquetaAtrib x = Just x

-- NOTE: 4 ex3 a
-- como as once iram executar até receberem um resultado positivo,
-- ou seja, trocar a estratégia não será suficiente para que estas abordagens
-- funcionem como esperado. Para estas estratégias ao invés de usar a função
-- idTP, que devolve um resultado positivo caso as funções antecedentes falhem,
-- usamos a failTp, de modo a que as estratégias falhem até modificarem
-- um nó da árvore.

-- NOTE: 4 ex 4

arithmeticOpt :: PicoC -> PicoC
arithmeticOpt p =
    let pZipper = toZipper p
        Just newp = applyTP (full_buTP step) pZipper
        step = idTP `adhocTP` arithmeticOptOne
     in fromZipper newp

arithmeticOptOne :: Exp -> Maybe Exp
arithmeticOptOne (Add (Const 0) x) = Just x
arithmeticOptOne (Add x (Const 0)) = Just x
arithmeticOptOne (Mult (Const 1) x) = Just x
arithmeticOptOne (Mult x (Const 1)) = Just x
arithmeticOptOne x = Just x

-- NOTE: 4 ex 6
-- o erro é que a função do exercício 4, por estar a usar full_tdTP não irá
-- otimizar a mesma expressão multiplas vezes, para isso usamos innermost que
-- irá otimizar a expressão até que não seja possível otimizar mais.
-- contraexemplo, x * 1 + 0,
fullArithmeticOpt :: PicoC -> PicoC
fullArithmeticOpt p =
    let pZipper = toZipper p
        Just newp = applyTP (innermost step) pZipper
        step = failTP `adhocTP` fullArithmeticOptOne
     in fromZipper newp

fullArithmeticOptOne :: Exp -> Maybe Exp
fullArithmeticOptOne (Add (Const 0) x) = Just x
fullArithmeticOptOne (Add x (Const 0)) = Just x
fullArithmeticOptOne (Mult (Const 1) x) = Just x
fullArithmeticOptOne (Mult x (Const 1)) = Just x
fullArithmeticOptOne x = Nothing

-- NOTE: exercicio Refactor Tpc Entrega 21-03
refactor :: PicoC -> PicoC
refactor p =
    let pZipper = toZipper p
        Just newp = applyTP (innermost step) pZipper
        step = failTP `adhocTPSeq` ifMerge `adhocTPSeq` expRedundant `adhocTPSeq` ifSwap
     in fromZipper newp

ifSwap :: Inst -> Maybe Inst
ifSwap (ITE (Not x) t e) = Just (ITE x e t)
ifSwap _ = Nothing

expRedundant :: Inst -> Maybe Inst
expRedundant (ITE (EQ x TRUE) t e) = Just (ITE x t e)
expRedundant (ITE (EQ x FALSE) t e) = Just (ITE (Not x) t e)
expRedundant (While (EQ x TRUE) t) = Just (While x t)
expRedundant (While (EQ x FALSE) t) = Just (While (Not x) t)
expRedundant _ = Nothing

ifMerge :: Inst -> Maybe Inst
ifMerge (ITE x1 [ITE x2 t2 []] e1) = Just (ITE (AND x1 x2) t2 e1)
ifMerge _ = Nothing

-- NOTE: Quickcheck Tpc Entrega 04-04 do prop
-- instance Arbitrary PicoC where
--     arbitrary = genPicoC
-- genPicoC :: Gen PicoC
-- genPicoC = do
--     x <- vectorOf 5 genInst
--     return (PicoC x)
--
-- genInst :: Gen Inst
-- genInst = frequency [(95, genAttrib), (2, genWhile), (3, genITE)]
--
-- genAttrib :: Gen Inst
-- genAttrib = do
--     x <- genVarName
--     Attrib x <$> genExp
--
-- genWhile :: Gen Inst
-- genWhile = do
--     x <- genExp
--     While x <$> genCBlock
--
-- genITE :: Gen Inst
-- genITE = do
--     x <- genExp
--     y <- genCBlock
--     ITE x y <$> genCBlock
--
-- genExp :: Gen Exp
-- genExp = frequency [genConst, genVar, genBool, genArithmeticExpo, genAND, genOR, genLT, genGT, genEQ, genNot]
--   where
--     genConst = (60, do Const <$> arbitrary)
--     genVar = (30, do Var <$> genVarName)
--     genBool = (10, elements [TRUE, FALSE])
--     genArithmeticExpo = (1, genArithmeticExp)
--     genAND = (1, do x <- genExp; AND x <$> genExp)
--     genOR = (1, do x <- genExp; OR x <$> genExp)
--     genLT = (1, do x <- genExp; LT x <$> genExp)
--     genGT = (1, do x <- genExp; GT x <$> genExp)
--     genEQ = (1, do x <- genExp; EQ x <$> genExp)
--     genNot = (1, do Not <$> genExp)
--
-- genArithmeticExp :: Gen Exp
-- genArithmeticExp = frequency [genConst, genVar, genAdd, genSub, genMult, genDiv, genNeg]
--   where
--     genConst = (80, do Const <$> arbitrary)
--     genVar = (15, do Var <$> genVarName)
--     genAdd = (1, do x <- genArithmeticExp; Add x <$> genArithmeticExp)
--     genSub = (1, do x <- genArithmeticExp; Sub x <$> genArithmeticExp)
--     genMult = (1, do x <- genArithmeticExp; Mult x <$> genArithmeticExp)
--     genDiv = (1, do x <- genArithmeticExp; Div x <$> genArithmeticExp)
--     genNeg = (1, do Neg <$> genArithmeticExp)
--
-- genVarName :: Gen String
-- genVarName = do
--     -- to make sure the var name is valid
--     f <- elements ['a' .. 'z']
--     r <- listOf (elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']))
--     return (f : r)
--
-- genCBlock :: Gen CBlock
-- genCBlock = vectorOf 5 genInst

-- NOTE: Check if a PicoC code is valid
prop_valid :: PicoC -> Bool
prop_valid p = p == picoC (unparse p)

-- NOTE: Check if refactor is idempotent
prop_refactor_idempotent :: PicoC -> Bool
prop_refactor_idempotent p = refactor p == refactor (refactor p)

-- NOTE: Check if optimazation is idempotent
prop_optimization_idempotent :: PicoC -> Bool
prop_optimization_idempotent p = arithmeticOpt p == arithmeticOpt (arithmeticOpt p)

-- NOTE: Evaluate
type Inputs = [(String, Int)]


removeItem :: (Eq a) => a -> [(a, b)] -> [(a, b)]
removeItem _ [] = []
removeItem k (c@(ck, v) : t)
    | k == ck = t
    | otherwise = c : removeItem k t

prog1 = picoC "if (a > 3) then { a = a + 1; }else { a = b; } return a;"
inputs1 :: Inputs
inputs1 = [("a", 2), ("b", 5)]

prog2 = picoC "while (a < 5) { a = a + 1;} return b;"
inputs2 :: Inputs
inputs2 = [("a", 0), ("b", 5)]

prog3 = picoC "if (!(a > b)) then { if (b > c) then { m = b; } else { m = c; } } else { if (a > c) then { m = a; } else { m = b; } } return m;"
inputs3 :: Inputs
inputs3 = [("a", 1), ("b", 5),("c", 3)]

prog4 = picoC "if ((a > 3) > (b < 4)) then { a = 1;} a = 5; b = 2; return a;"
prog5 = picoC "a = (a > 3) > (b < 4);"
evaluate :: PicoC -> Inputs -> Int
evaluate (PicoC i) inp = ret
  where
   (c, Just ret) = runPicoC i inp

-- NOTE: eval exp needs to work with logical statements
runPicoC :: [Inst] -> Inputs -> (Inputs, Maybe Int)
runPicoC [] i = (i, Nothing)
runPicoC ((Return x) : _) i = (i, Just (eval x i))
runPicoC ((Attrib n v) : t) i = runPicoC t ((n, eval v i) : removeItem n i)
runPicoC ((ITE exp e1 e2) : t) i
    | eval exp i /= 0 = if isJust ret_e2 then i_e2 else runPicoC t inputs_e1
    | otherwise = if isJust ret_e2 then i_e2 else runPicoC t inputs_e2
  where
    i_e1@(inputs_e1, ret_e1) = runPicoC e1 i
    i_e2@(inputs_e2, ret_e2) = runPicoC e2 i
runPicoC inst@((While exp c) : t) i
    | eval exp i /= 0 = if isJust ret then i_while else runPicoC inst inp_while
    | otherwise = runPicoC t i
  where
    i_while@(inp_while, ret) = runPicoC c i

runTest :: PicoC -> (Inputs, Int) -> Bool
runTest p (i, result) = evaluate p i == result

runTestSuite :: PicoC -> [(Inputs, Int)] -> Bool
runTestSuite _ [] = True
runTestSuite p (h:t) = runTest p h && runTestSuite p t





-- Example
examplePicoC = "margem = 15; \n if (!(margem > 3)) then { \n margem = 4 * 23 + 3 ; \n} else {\n margem = x * 1 + 0; \n}"
examplePicoC2 = "margem = 15; \n if (margem > 3) then { \n margem = 4 * 23 + 3 ; \n}"

refactorTest = "margem = 15; if (margem == TRUE) then {if (!(margem > 3)) then {} else {margem = 4 * 23 + 3 ;}} else {margem = 2;}"
