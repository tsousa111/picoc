{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Generator where
import Test.QuickCheck
import Prelude hiding (EQ,LT,GT)
import PicoC

instance Arbitrary PicoC where
    arbitrary = genPicoC

genPicoC :: Gen PicoC
genPicoC = do
    x <- vectorOf 5 genInst
    return (PicoC x)

genInst :: Gen Inst
genInst = frequency [(95, genAttrib), (2, genWhile), (3, genITE)]

genAttrib :: Gen Inst
genAttrib = do
    var <- genVarName
    exp <- genExp
    return $ Attrib var exp

genWhile :: Gen Inst
genWhile = do
    exp <- genExp
    cBlock <- genCBlock
    return $ While exp cBlock

genITE :: Gen Inst
genITE = do
    exp <- genExp
    t <- genCBlock
    e <- genCBlock
    return $ ITE exp t e

genExp :: Gen Exp
genExp = frequency [genConst, genVar, genBool, genArithmeticExpo, genAND, genOR, genLT, genGT, genEQ, genNot]
  where
    genConst = (60, do x <- arbitrary; return $ Const x)
    genVar = (30, do x <- genVarName; return $ Var x)
    genBool = (10, elements [TRUE, FALSE])
    genArithmeticExpo = (1, genArithmeticExp)
    genAND = (1, do x <- genExp; y <- genExp ; return $ AND x y)
    genOR = (1, do x <- genExp; y <- genExp ; return $ OR x y)
    genLT = (1, do x <- genExp; y <- genExp ; return $ LT x y)
    genGT = (1, do x <- genExp; y <- genExp ; return $ GT x y)
    genEQ = (1, do x <- genExp; y <- genExp ; return $ EQ x y)
    genNot = (1, do x <- genExp; return $ Not x)

genArithmeticExp :: Gen Exp
genArithmeticExp = frequency [genConst, genVar, genAdd, genSub, genMult, genDiv, genNeg]
  where
    genConst = (80, do x <- arbitrary; return $ Const x)
    genVar = (15, do x <- genVarName; return $ Var x)
    genAdd = (1, do x <- genArithmeticExp;y <- genArithmeticExp; return $ Add x y)
    genSub = (1, do x <- genArithmeticExp;y <- genArithmeticExp; return $ Sub x y)
    genMult = (1, do x <- genArithmeticExp;y <- genArithmeticExp; return $ Mult x y)
    genDiv = (1, do x <- genArithmeticExp;y <- genArithmeticExp; return $ Div x y)
    genNeg = (1, do x <- genArithmeticExp; return $ Neg x )

genVarName :: Gen String
genVarName = do
    -- to make sure the var name is valid
    f <- elements ['a' .. 'z']
    r <- listOf (elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']))
    return (f : r)

genCBlock :: Gen CBlock
genCBlock = vectorOf 5 genInst
