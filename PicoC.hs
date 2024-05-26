{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Redundant return" #-}

module PicoC where

import Debug.Trace
import Data.Char
import Data.Data
import Data.Maybe
import Library.StrategicData (StrategicData)
import Library.Parser
import Test.QuickCheck
import Text.Read
import Prelude hiding (EQ, GT, LT, (<$>), (<*>), (<|>))
import System.Random
import Library.Ztrategic

instance StrategicData PicoC
instance (StrategicData a) => StrategicData [a]

data PicoC = PicoC [Inst]
    deriving (Data, Eq)

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
pIf = f <$> token' "if" <*> symbol' '(' <*> pExp <*> symbol' ')' <*> token' "then" <*> pCBlock <*> token' "else" <*> pCBlock
   <|> g <$> token' "if" <*> symbol' '(' <*> pExp <*> symbol' ')' <*> token' "then" <*> pCBlock
  where
    f _ _ x _ _ y _ z = ITE x y z
    g _ _ x _ _ y = ITE x y []

pReturn :: Parser Inst
pReturn = f <$> token' "return" <*> pExp <*> symbol' ';'
  where
    f _ x _ = Return x

pPrint :: Parser Inst
pPrint = f <$> token' "print" <*> symbol' '(' <*> oneOrMore (satisfy' isAscii) <*> symbol' ')' <*> symbol' ';'
    where
        f _ _ x _ _ = Print x

pInst :: Parser Inst
pInst = pIf <|> pReturn <|> pWhile <|> pAtrib <|> pPrint

pPicoC :: Parser PicoC
pPicoC = PicoC <$> oneOrMore pInst

picoC :: String -> PicoC
picoC s = fst $ head $ filter ((== "") . snd) (pPicoC s)

---------------------------
-- unparse : AST to text
-- pretty printing
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
unparseExp (EQ x y) = "(" ++ unparseExp x ++ " == " ++ unparseExp y ++ ")"
unparseExp (Not x) = "!" ++ "(" ++ unparseExp x ++ ")"

unparseCBlock :: CBlock -> String
unparseCBlock x = "{\n" ++ unparseListInst x ++ "}\n"

-- NOTE: funcao prop do tpc 22/2
prop :: PicoC -> Bool
prop ast = ast == picoC (unparse ast)

-- NOTE: Exp eval
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
eval (AND e1 e2) c = if eval e1 c /= 0 && eval e2 c /= 0 then 1 else 0
eval (OR e1 e2) c = if eval e1 c /= 0 || eval e2 c /= 0 then 1 else 0
eval (Not e) c = if eval e c == 0 then 1 else 0


-- NOTE: Evaluate
type Inputs = [(String, Int)]


removeItem :: (Eq a) => a -> [(a, b)] -> [(a, b)]
removeItem _ [] = []
removeItem k (c@(ck, v) : t)
    | k == ck = t
    | otherwise = c : removeItem k t

-- Subtração entre 2 números positivos, -1 em caso de erro de input (demora muito)
prog1 = picoC "if((a > -1) && (b > -1))then{if(a>b)then{c = a - b;}else{c = b - a;}}else{c = -1;}return c;"

-- Conta arbitrária
prog2 = picoC "if (a>10)then {a = a * 10;}else{a = a - 10;}return a;"

-- Maior de 3 números, resultado em m
prog3 = picoC "if (a > b) then { if (a > c) then { m = a; } else { m = c; } } else { if (b > c) then { m = b; } else { m = c; } } return m;"

inputs_prog1_1 = [("a", 1),("b",10)]
inputs_prog1_2 = [("a", 10),("b",10)]
inputs_prog1_3 = [("a", -1),("b",10)]

inputsprog_2_1 = [("a", 20)]
inputsprog_2_2 = [("a", 0)]
inputsprog_2_3 = [("a", 3)]

inputsprog_3_1 = [("a", 2), ("b", 0),("c", 5)]
inputsprog_3_2 = [("a", 1), ("b", 6),("c", 3)]
inputsprog_3_3 = [("a", 5), ("b", 3),("c", 1)]


testSuiteProg1 = [(inputs_prog1_1,9),(inputs_prog1_2, 0), (inputs_prog1_3, -1)]
runTestSuiteProg1 = runTestSuite prog1 testSuiteProg1

testSuiteProg2 = [(inputsprog_2_1,200),(inputsprog_2_2, -10), (inputsprog_2_3, -7)]
runTestSuiteProg2 = runTestSuite prog2 testSuiteProg2


testSuiteProg3 = [(inputsprog_3_1, 5), (inputsprog_3_2, 6), (inputsprog_3_3, 5)]
runTestSuiteProg3 = runTestSuite prog3 testSuiteProg3

evaluate :: PicoC -> Inputs -> Int
evaluate (PicoC i) inp = ret
  where
   (c, Just ret) = runPicoC i inp

-- NOTE: eval exp needs to work with logical statements
runPicoC :: [Inst] -> Inputs -> (Inputs, Maybe Int)
runPicoC [] i = (i, Nothing)
runPicoC ((Return x) : _) i = (i, Just (eval x i))
runPicoC ((Print x): t) i = trace x (runPicoC t i)
runPicoC ((Attrib n v) : t) i = runPicoC t ((n, eval v i) : removeItem n i)
runPicoC ((ITE exp e1 e2) : t) i
    | eval exp i /= 0 = if isJust ret_e1 then i_e1 else runPicoC t inputs_e1
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

runTests :: PicoC -> [(Inputs,Int)] -> [Bool]
runTests _ [] = []
runTests p (h:t) = runTest p h : runTests p t

runTestSuite :: PicoC -> [(Inputs, Int)] -> Bool
runTestSuite p l = and $ runTests p l

-- NOTE: Instrumentation 8)
instrumentationInst :: [Inst] -> [Inst]
instrumentationInst [] = []
instrumentationInst (a@(Attrib n v) : t) = a : Print ("> attrib " ++ show n ++ " = "++ unparseExp v) : instrumentationInst t
instrumentationInst ((ITE x y z) : t) = Print ("> if condition " ++ unparseExp x) : ITE x (instrumentationInst y) (instrumentationInst z) : instrumentationInst t
instrumentationInst ((While x c): t) = Print ("> while condition " ++ unparseExp x) : While x (instrumentationInst c) : instrumentationInst t
instrumentationInst (r@(Return x): _) = Print ("> return " ++ unparseExp x ++ "\n") : [r]

instrumentation :: PicoC -> PicoC
instrumentation (PicoC inst) = PicoC (instrumentationInst inst)

-- NOTE: Instrumentation 9)
instrumentedTestSuite :: PicoC -> [(Inputs, Int)] -> [Bool]
instrumentedTestSuite p i = runTests (instrumentation p) i

breakCode :: Inst -> Maybe Inst
breakCode (Attrib v n) = Just $ Attrib v (Neg n)
breakCode (While exp b) = Just $ While (Not exp) b
breakCode (ITE exp t e) = Just $ ITE (Not exp) t e
breakCode _ = Nothing

mutateCode :: PicoC -> Gen PicoC
mutateCode p = do
    let muts = mutations p breakCode
    x <- choose (0, length muts - 1)
    let new = muts !! x
    return new

mutateCodeSeed:: PicoC -> Int -> PicoC
mutateCodeSeed p@(PicoC i) s = muts !! x
    where
        muts = mutations p breakCode
        gen = mkStdGen s -- seed
        (x, _) = randomR (0, length muts - 1) gen


runMutationSuite :: PicoC -> [(Inputs, Int)] -> Int -> IO [Bool]
runMutationSuite p l s = do
                let mutation = mutateCodeSeed p s
                _ <- print mutation
                let result = instrumentedTestSuite mutation l
                return result

runMutationSuiteProg1 s = runMutationSuite prog1 testSuiteProg1 s
runMutationSuiteProg2 s = runMutationSuite prog2 testSuiteProg2 s
runMutationSuiteProg3 s = runMutationSuite prog3 testSuiteProg3 s

