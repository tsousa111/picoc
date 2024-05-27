module TestSuite where

import Data.Maybe
import Debug.Trace
import Library.Ztrategic
import PicoTypes
import PicoUnparser
import System.Random
import Test.QuickCheck
import Prelude hiding (EQ, GT, LT)

type Inputs = [(String, Int)]

removeItem :: (Eq a) => a -> [(a, b)] -> [(a, b)]
removeItem _ [] = []
removeItem k (c@(ck, v) : t)
    | k == ck = t
    | otherwise = c : removeItem k t

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
eval (GET e1 e2) c = if eval e1 c >= eval e2 c then 1 else 0
eval (LET e1 e2) c = if eval e1 c <= eval e2 c then 1 else 0
eval (DIFF e1 e2) c = if eval e1 c /= eval e2 c then 1 else 0
eval (EQ e1 e2) c = if eval e1 c == eval e2 c then 1 else 0
eval (AND e1 e2) c = if eval e1 c /= 0 && eval e2 c /= 0 then 1 else 0
eval (OR e1 e2) c = if eval e1 c /= 0 || eval e2 c /= 0 then 1 else 0
eval (Not e) c = if eval e c == 0 then 1 else 0

evaluate :: PicoC -> Inputs -> Int
evaluate (PicoC i) inp = ret
  where
    (c, Just ret) = runPicoC i inp

-- NOTE: eval exp needs to work with logical statements
runPicoC :: [Inst] -> Inputs -> (Inputs, Maybe Int)
runPicoC [] i = (i, Nothing)
runPicoC ((Return x) : _) i = (i, Just (eval x i))
runPicoC ((Print x) : t) i = trace x (runPicoC t i)
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

runTests :: PicoC -> [(Inputs, Int)] -> [Bool]
runTests _ [] = []
runTests p (h : t) = runTest p h : runTests p t

runTestSuite :: PicoC -> [(Inputs, Int)] -> Bool
runTestSuite p l = and $ runTests p l

-- NOTE: Instrumentation 8)
instrumentationInst :: [Inst] -> [Inst]
instrumentationInst [] = []
instrumentationInst (a@(Attrib n v) : t) = a : Print ("> attrib " ++ show n ++ " = " ++ unparseExp v) : instrumentationInst t
instrumentationInst ((ITE x y z) : t) = Print ("> if condition " ++ unparseExp x) : ITE x (instrumentationInst y) (instrumentationInst z) : instrumentationInst t
instrumentationInst ((While x c) : t) = Print ("> while condition " ++ unparseExp x) : While x (instrumentationInst c) : instrumentationInst t
instrumentationInst (r@(Return x) : _) = Print ("> return " ++ unparseExp x ++ "\n") : [r]

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

mutateCodeSeed :: PicoC -> Int -> PicoC
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
