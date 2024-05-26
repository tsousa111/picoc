module Refactor where

import Data.Generics.Zipper
import Library.Ztrategic
import PicoC
import Prelude hiding (EQ, GT, LT, (<$>))

-- NOTE: Vanilla optimization
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

-- NOTE: Check if a PicoC code is valid
prop_valid :: PicoC -> Bool
prop_valid p = p == picoC (unparse p)

-- NOTE: Check if refactor is idempotent
prop_refactor_idempotent :: PicoC -> Bool
prop_refactor_idempotent p = refactor p == refactor (refactor p)

-- NOTE: Check if optimazation is idempotent
prop_optimization_idempotent :: PicoC -> Bool
prop_optimization_idempotent p = arithmeticOpt p == arithmeticOpt (arithmeticOpt p)
